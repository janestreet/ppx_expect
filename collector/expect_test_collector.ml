open Expect_test_common.Std

module List = ListLabels

module Test_outcome = struct
  type t =
    { file_digest     : File.Digest.t
    ; location        : File.Location.t
    ; expectations    : Expectation.Raw.t list
    ; saved_output    : (File.Location.t * string) list
    ; trailing_output : string
    }
end

let tests_run : Test_outcome.t list ref = ref []

let protect ~finally ~f =
  match f () with
  | x           -> finally (); x
  | exception e -> finally (); raise e
;;

module Current_file = struct
  let current = ref None

  (* Tests in the current file *)
  let tests : (File.Location.t, unit) Hashtbl.t = Hashtbl.create 64

  let set ~absolute_filename =
    match !current with
    | None -> current := Some absolute_filename
    | Some _ ->
      failwith "Expect_test_collector.set: already set"
  ;;

  let unset () =
    match !current with
    | Some _ -> current := None; Hashtbl.clear tests
    | None ->
      failwith "Expect_test_collector.unset: not set"
  ;;

  let get () =
    match !current with
    | Some fn -> fn
    | None ->
      failwith "Expect_test_collector.get: not set"
  ;;

  let add_test loc =
    if Hashtbl.mem tests loc then
      Printf.ksprintf failwith
        !"Trying to run the same expect test too many times.\n\
          Expect tests can only run once as they can have only one correction.\n\
          The test is defined at %{File.Name}:%d"
        loc.filename loc.line_number
    else
      Hashtbl.add tests loc ()
end

module Make(C : Expect_test_config.S) = struct
  let ( >>= ) t f = C.IO.bind t ~f
  let return = C.IO.return

  module C = struct
    include C
    let flush () =
      (* Always flush [Pervasives.stdout] *)
      Pervasives.flush Pervasives.stdout;
      C.flush ()
  end

  module Instance : sig
    type t

    val save_output : t -> File.Location.t -> unit C.IO.t

    val exec :
      file_digest    : File.Digest.t ->
      location       : File.Location.t ->
      expectations   : Expectation.Raw.t list ->
      f              : (t -> unit C.IO.t) ->
      unit
  end = struct
    module Running : sig
      type t
      val create : unit -> t
      val get_outputs_and_cleanup : t -> (File.Location.t * string) list  * string
      val save_output : t -> File.Location.t -> unit
    end = struct
      type t =
        { mutable saved : (File.Location.t * int) list
        ; chan          : out_channel
        ; filename      : File.Name.t
        }

      external swap : out_channel -> out_channel -> unit = "caml_out_channel_swap_fd"
      external pos_out : out_channel -> int = "caml_out_channel_pos_fd"

      let get_position () = pos_out stdout
      ;;

      let create () =
        let filename = Filename.temp_file "expect-test" "stdout" in
        let chan = open_out filename in
        swap chan stdout;
        { chan
        ; filename = File.Name.of_string filename
        ; saved    = []
        }
      ;;

      let get_outputs_and_cleanup t =
        let last_ofs = get_position () in
        swap t.chan stdout;
        close_out t.chan;

        let fname = File.Name.relative_to ~dir:(File.initial_dir ()) t.filename in
        protect ~finally:(fun () -> Sys.remove fname) ~f:(fun () ->
          let ic = open_in fname in
          protect ~finally:(fun () -> close_in ic) ~f:(fun () ->
            let ofs, outputs =
              List.fold_left (List.rev t.saved) ~init:(0, [])
                ~f:(fun (ofs, acc) (loc, next_ofs) ->
                  let s = really_input_string ic (next_ofs - ofs) in
                  (next_ofs, ((loc, s) :: acc)))
            in
            let trailing_output = really_input_string ic (last_ofs - ofs) in
            (outputs, trailing_output)))
      ;;

      let save_output running location =
        let pos = get_position () in
        running.saved <- (location, pos) :: running.saved

    end

    type state = Running of Running.t | Ended
    type t = { mutable state : state }

    let rec final_flush ?(count=0) k =
      let max_attempts = 10 in
      C.flush () >>= fun () ->
      if C.flushed () then
        k ~append:""
      else if count = max_attempts then
        k ~append:(Printf.sprintf
                     "\nSTOPPED COLLECTING OUTPUT AFTER %d FLUSHING ATTEMPS\n\
                      THERE MUST BE A BACKGROUND JOB PRINTING TO STDOUT\n"
                     max_attempts)
      else
        final_flush ~count:(count + 1) k

    let exec ~file_digest ~location ~expectations ~f =
      let running = Running.create () in
      let t = { state = Running running } in
      let finally () =
        C.run (fun () ->
          final_flush (fun ~append ->
            t.state <- Ended;
            let saved_output, trailing_output = Running.get_outputs_and_cleanup running in
            tests_run :=
              { file_digest
              ; location
              ; expectations
              ; saved_output
              ; trailing_output = trailing_output ^ append
              } :: !tests_run;
            return ()))
      in
      protect ~finally ~f:(fun () -> C.run (fun () -> f t))
    ;;

    let save_output t location =
      match t.state with
      | Running running ->
        C.flush () >>= fun () ->
        Running.save_output running location;
        return ()
      | Ended ->
        Printf.ksprintf failwith
          !"Expect_test_collector.Instance.save_output called after test has ended \
            (loc = %{sexp:File.Location.t})"
          location
    ;;
  end

  let run
        ~file_digest
        ~(location:File.Location.t)
        ~absolute_filename:defined_in
        ~description
        ~tags
        ~expectations
        ~inline_test_config
        f
    =
    let registering_tests_for = Current_file.get () in
    Ppx_inline_test_lib.Runtime.test
      ~config:inline_test_config
      ~descr:(match description with None -> "" | Some s -> ": " ^ s)
      ~tags
      ~filename:(File.Name.to_string location.filename)
      ~line_number:location.line_number
      ~start_pos:(location.start_pos - location.line_start)
      ~end_pos:(location.end_pos   - location.line_start)
      (fun () ->
         Current_file.add_test location;
         if defined_in <> registering_tests_for then
           Printf.ksprintf failwith
             "Trying to run an expect test from the wrong file.\n\
              - test declared at %s:%d\n\
              - trying to run it from %s\n"
             defined_in location.line_number registering_tests_for
         else begin
           (* To avoid capturing not-yet flushed data of the stdout buffer *)
           C.run C.flush;
           Instance.exec ~file_digest ~location ~expectations ~f;
           true
         end
      );
  ;;
end

let tests_run () = !tests_run
