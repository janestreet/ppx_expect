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

module Make(C : Expect_test_config.S) = struct
  let ( >>= ) = C.IO.bind
  let return = C.IO.return

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
    module Running = struct
      type t =
        { mutable saved : (File.Location.t * int) list
        ; stdout_backup : Unix.file_descr
        ; fd            : Unix.file_descr
        ; filename      : File.Name.t
        } [@@deriving fields]

      let create () =
        let stdout_backup = Unix.dup Unix.stdout in
        let filename = Filename.temp_file "expect-test" "stdout" in
        let fd = Unix.openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
        (* To avoid capturing not-yet flushed data of the stdout buffer *)
        flush stdout;
        Unix.dup2 fd Unix.stdout;
        { stdout_backup
        ; fd
        ; filename = File.Name.of_string filename
        ; saved    = []
        }
      ;;

      let get_position t =
        flush stdout;
        Unix.lseek t.fd 0 SEEK_CUR;
      ;;

      let get_outputs_and_cleanup t =
        let last_ofs = get_position t in
        Unix.close t.fd;
        Unix.dup2 t.stdout_backup Unix.stdout;
        Unix.close t.stdout_backup;

        let fname = File.Name.to_string t.filename in
        protect ~finally:(fun () -> Unix.unlink fname) ~f:(fun () ->
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
    end

    type state = Running of Running.t | Ended
    type t = { mutable state : state }

    let exec ~file_digest ~location ~expectations ~f =
      let running = Running.create () in
      let t = { state = Running running } in
      let finally () =
        C.run (fun () ->
          C.flush () >>= fun () ->
          t.state <- Ended;
          let saved_output, trailing_output = Running.get_outputs_and_cleanup running in
          tests_run :=
            { file_digest
            ; location
            ; expectations
            ; saved_output
            ; trailing_output
            } :: !tests_run;
          return ())
      in
      protect ~finally ~f:(fun () -> C.run (fun () -> f t))
    ;;

    let save_output t location =
      match t.state with
      | Running running ->
        C.flush () >>= fun () ->
        let pos = Running.get_position running in
        running.saved <- (location, pos) :: running.saved;
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
        ~description
        ~expectations
        ~inline_test_config
        f
    =
    Ppx_inline_test_lib.Runtime.test
      inline_test_config
      (match description with None -> "" | Some s -> ": " ^ s)
      (File.Name.to_string location.filename)
      location.line_number
      (location.start_pos - location.line_start)
      (location.end_pos   - location.line_start)
      (fun () ->
         C.run C.flush;
         Instance.exec ~file_digest ~location ~expectations ~f;
         true
      );
  ;;
end

let tests_run () = !tests_run
