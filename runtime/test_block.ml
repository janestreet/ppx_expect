open! Base
open Types

(* [Shared] and [Configured] primarily contain boilerplate involving the FFI and printing
   [CR]s. The interesting logic is in [Make]. *)

module Shared : sig
  type t

  val src_filename : t -> string
  val output_file : t -> string
  val failure_ref : t -> bool ref
  val set_up_block : string -> t
  val read_test_output_unsanitized : t -> string
  val flush : unit -> unit
  val clean_up_block : t -> unit
end = struct
  type t =
    { src_filename : string
    ; output_file : string
    ; fail : bool ref
    ; test_output_reader : Stdlib.in_channel
    ; test_output_writer : Stdlib.out_channel
    ; old_offset : int ref
    }

  let src_filename { src_filename; _ } = src_filename
  let output_file { output_file; _ } = output_file
  let failure_ref { fail; _ } = fail

  external redirect_stdout
    :  output:Stdlib.out_channel
    -> stdout:Stdlib.out_channel
    -> stderr:Stdlib.out_channel
    -> unit
    = "ppx_expect_runtime_before_test"

  external restore_stdout
    :  stdout:Stdlib.out_channel
    -> stderr:Stdlib.out_channel
    -> unit
    = "ppx_expect_runtime_after_test"

  external pos_out : Stdlib.out_channel -> int = "ppx_expect_runtime_out_channel_position"
  external flush_stubs : unit -> unit = "ppx_expect_runtime_flush_stubs_streams"

  (* Save std file descriptors, open a temp file for test output, and reroute stdout and
     stderr there. *)
  let set_up_block src_filename =
    let output_file =
      Current_file.absolute_path (Stdlib.Filename.temp_file "expect-test" "output")
    in
    let test_output_writer =
      Stdlib.open_out_gen [ Open_wronly; Open_creat; Open_binary ] 0o644 output_file
    in
    let test_output_reader = Stdlib.open_in_bin output_file in
    redirect_stdout ~output:test_output_writer ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    { src_filename
    ; output_file
    ; test_output_reader
    ; test_output_writer
    ; old_offset = ref 0
    ; fail = ref false
    }
  ;;

  (* Close the temp file and restore stdout and stderr. *)
  let clean_up_block { output_file; test_output_reader; test_output_writer; _ } =
    Stdlib.close_in test_output_reader;
    restore_stdout ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    Stdlib.close_out test_output_writer;
    Stdlib.Sys.remove output_file
  ;;

  let flush () =
    Stdlib.Format.pp_print_flush Stdlib.Format.std_formatter ();
    Stdlib.Format.pp_print_flush Stdlib.Format.err_formatter ();
    Stdlib.flush Stdlib.stdout;
    Stdlib.flush Stdlib.stderr;
    flush_stubs ()
  ;;

  let read_test_output_unsanitized { test_output_reader; old_offset; _ } =
    let new_offset =
      flush ();
      pos_out Stdlib.stdout
    in
    let len = new_offset - !old_offset in
    old_offset := new_offset;
    Stdlib.really_input_string test_output_reader len
  ;;
end

module Configured (C : Expect_test_config_types.S) = struct
  let cr_prefix =
    match C.upon_unreleasable_issue with
    | `CR -> "CR "
    | `Warning_for_collector_testing -> ""
  ;;

  let cr_for_backtrace =
    Printf.sprintf
      {|(* %sexpect_test_collector: This test expectation appears to contain a backtrace.
   This is strongly discouraged as backtraces are fragile.
   Please change this test to not include a backtrace. *)|}
      cr_prefix
  ;;

  let cr_for_multiple_outputs ~output_name ~outputs =
    let cr_body =
      Printf.sprintf "Test ran multiple times with different %ss" output_name
    in
    let cr = Printf.sprintf "(* %sexpect_test: %s *)" cr_prefix cr_body in
    let num_outputs = List.length outputs in
    let header index =
      let header = Printf.sprintf "=== Output %d / %d ===" (index + 1) num_outputs in
      let pad_length = String.length cr - String.length header in
      if pad_length <= 0
      then header
      else (
        let lpad = String.make (pad_length / 2) '=' in
        let rpad = String.make (pad_length - (pad_length / 2)) '=' in
        Printf.sprintf "%s%s%s" lpad header rpad)
    in
    let outputs_with_headers =
      List.concat_mapi outputs ~f:(fun index output -> [ header index; output ])
    in
    String.concat (cr :: outputs_with_headers) ~sep:"\n"
  ;;

  let sanitize = C.sanitize

  let check_for_backtraces s =
    if List.exists
         ~f:(fun substring -> String.is_substring ~substring s)
         [ "Raised at "; "Called from "; "Raised by primitive operation " ]
    then cr_for_backtrace ^ "\n\n" ^ s
    else s
  ;;

  let dump_backtrace possible_exn =
    match C.run possible_exn with
    | exception exn ->
      let bt = Stdlib.Printexc.get_raw_backtrace () in
      let exn_string =
        try Exn.to_string exn with
        | _ ->
          let name =
            Stdlib.Obj.Extension_constructor.of_val exn
            |> Stdlib.Obj.Extension_constructor.name
          in
          Printf.sprintf "(\"%s(Cannot print more details, Exn.to_string failed)\")" name
      in
      Some
        (match Stdlib.Printexc.raw_backtrace_to_string bt with
         | "" -> exn_string
         | bt -> String.concat ~sep:"\n" [ cr_for_backtrace; exn_string; bt ])
    | _ -> None
  ;;
end

(* The expect test currently being executed and some info we print if the program
   crashes in the middle of a test. *)
module Current_test : sig
  type t =
    { line_number : int
    ; basename : string
    ; location : Compact_loc.t
    ; test_block : Shared.t
    }

  val set : t -> unit
  val unset : unit -> unit
  val is_running : unit -> bool
  val current_test : unit -> Shared.t option
  val current_test_exn : unit -> Shared.t
  val iter : f:(t -> unit) -> unit
  val assert_no_test_running : basename:string -> line_number:int -> unit
end = struct
  type t =
    { line_number : int
    ; basename : string
    ; location : Compact_loc.t
    ; test_block : Shared.t
    }

  let test_is_running : t option ref = ref None
  let set t = test_is_running := Some t
  let unset () = test_is_running := None
  let is_running () = Option.is_some !test_is_running

  let current_test () =
    Option.map !test_is_running ~f:(fun { test_block; _ } -> test_block)
  ;;

  let current_test_exn () = Option.value_exn (current_test ())
  let iter ~f = Option.iter !test_is_running ~f

  let assert_no_test_running ~basename ~line_number =
    iter
      ~f:
        (fun
          { line_number = outer_line_number
          ; basename = outer_basename
          ; location = _
          ; test_block = _
          }
          ->
      let sexp_here ~basename ~line_number : Sexp.t =
        List
          [ List [ Atom "file"; sexp_of_string basename ]
          ; List [ Atom "line"; sexp_of_int line_number ]
          ]
      in
      raise_s
        (Sexp.message
           "Expect_test_runtime: reached one [let%expect_test] from another. Nesting \
            expect\n\
            tests is prohibited."
           [ ( "outer_test"
             , sexp_here ~basename:outer_basename ~line_number:outer_line_number )
           ; "inner_test", sexp_here ~basename ~line_number
           ]))
  ;;
end

(* The main testing functions of a test block, which depend on configurations. *)
module Make (C : Expect_test_config_types.S) = struct
  module Configured = Configured (C)

  let read_test_output_no_backtrace_check () =
    Current_test.current_test_exn ()
    |> Shared.read_test_output_unsanitized
    |> Configured.sanitize
  ;;

  let read_test_output_sanitized_and_checked () =
    read_test_output_no_backtrace_check () |> Configured.check_for_backtraces
  ;;

  let run_test_inner ~test_id ~test_output_raw t =
    Test_node.record_result
      ~expect_node_formatting:Expect_node_formatting.default
      ~failure_ref:(Shared.failure_ref t)
      ~test_output_raw
      (Test_node.Global_results_table.find_test
         ~absolute_filename:(Shared.src_filename t)
         ~test_id)
  ;;

  let run_test ~test_id =
    Current_test.current_test_exn ()
    |> run_test_inner
         ~test_id
         ~test_output_raw:(read_test_output_sanitized_and_checked ())
  ;;

  let run_suite
    ~filename_rel_to_project_root
    ~line_number
    ~(location : Compact_loc.t)
    ~(trailing_loc : Compact_loc.t)
    ~(body_loc : Compact_loc.t)
    ~formatting_flexibility
    ~expected_exn
    ~trailing_test_id
    ~exn_test_id
    ~description
    ~tags
    ~inline_test_config
    ~expectations
    f
    =
    let ({ start_bol; start_pos; end_pos } : Compact_loc.t) = location in
    let basename = Stdlib.Filename.basename filename_rel_to_project_root in
    (* Even if the current tag set indicates this test should be dropped, check that it
       wasn't reached from another expect test *)
    Current_test.assert_no_test_running ~basename ~line_number;
    Ppx_inline_test_lib.test
      ~config:inline_test_config
      ~descr:(lazy (Option.value description ~default:""))
      ~tags
      ~filename:basename
      ~line_number
      ~start_pos:(start_pos - start_bol)
      ~end_pos:(end_pos - start_bol)
      (fun () ->
        (* Check that the test is being run from the file in which it was defined *)
        Current_file.verify_that_file_is_current_exn
          ~line_number
          ~filename_rel_to_project_root;
        let absolute_filename = Current_file.absolute_path basename in
        (* Create the tests for trailing output and uncaught exceptions *)
        let expectations =
          let trailing_test =
            Expectation.expect_trailing
              ~insert_loc:
                { loc = { trailing_loc with end_pos = trailing_loc.start_pos }; body_loc }
            |> Test_node.of_expectation
          in
          let exn_test =
            match expected_exn with
            | Some _ ->
              Expectation.expect_uncaught_exn
                ~formatting_flexibility
                ~located_payload:expected_exn
                ~node_loc:trailing_loc
              |> Test_node.of_expectation
            | None ->
              Expectation.expect_no_uncaught_exn
                ~insert_loc:{ loc = trailing_loc; body_loc }
              |> Test_node.of_expectation
          in
          (exn_test_id, exn_test) :: (trailing_test_id, trailing_test) :: expectations
        in
        (* Add the tests to the global table and reset their [reached_this_run] flags *)
        let expectations =
          Test_node.Global_results_table.initialize_and_register_tests
            ~absolute_filename
            expectations
            (fun ~original_file_contents ts ->
            List.concat_map
              ts
              ~f:
                (Test_node.For_mlt.to_diffs
                   ~cr_for_multiple_outputs:Configured.cr_for_multiple_outputs
                   ~expect_node_formatting:Expect_node_formatting.default
                   ~original_file_contents))
        in
        (* To avoid capturing not-yet flushed data of the stdout/stderr buffers. *)
        Shared.flush ();
        (* Redirect stdout/stderr *)
        let test_block = Shared.set_up_block absolute_filename in
        (* Run the test *)
        Current_test.set { line_number; basename; location; test_block };
        let test_exn =
          Configured.dump_backtrace (fun () ->
            (* Ignore output that was printed before the test started *)
            let (_ : string) = Shared.read_test_output_unsanitized test_block in
            f ())
        in
        (* Run the trailing output and uncaught exn test *)
        let test_output, test_to_run =
          let trailing_output =
            let trailing_raw = read_test_output_sanitized_and_checked () in
            match String.strip trailing_raw with
            | "" -> None
            | _ -> Some trailing_raw
          in
          match test_exn with
          | None -> Option.value trailing_output ~default:"", trailing_test_id
          | Some test_exn ->
            let test_output =
              match trailing_output with
              | None -> test_exn
              | Some trailing_output ->
                String.concat
                  ~sep:"\n"
                  [ test_exn; "Trailing output"; "---------------"; trailing_output ]
            in
            test_output, exn_test_id
        in
        run_test_inner test_block ~test_output_raw:test_output ~test_id:test_to_run;
        (* Perform the per-test reachability check *)
        List.iter expectations ~f:(fun (_, test_node) ->
          Test_node.record_end_of_run test_node);
        (* Restore stdout/stderr *)
        Shared.clean_up_block test_block;
        Current_test.unset ();
        (* Report that this test passed, because we report expect test failures by a
            different mechanism. *)
        true)
  ;;
end

let at_exit () =
  Current_test.iter
    ~f:
      (fun
        { line_number
        ; basename
        ; location = { start_bol; start_pos; end_pos }
        ; test_block
        }
        ->
    Shared.flush ();
    let fin = Stdlib.open_in_bin (Shared.output_file test_block) in
    let all_out = Stdlib.really_input_string fin (Stdlib.in_channel_length fin) in
    Shared.clean_up_block test_block;
    Stdlib.Printf.eprintf
      "File %S, line %d, characters %d-%d:\n\
       Error: program exited while expect test was running!\n\
       Output captured so far:\n\
       %s\n\
       %!"
      basename
      line_number
      (start_pos - start_bol)
      (end_pos - start_bol)
      all_out)
;;

module For_external = struct
  let read_current_test_output_exn ~here =
    match Current_test.current_test () with
    | Some test_block ->
      test_block |> Shared.read_test_output_unsanitized |> Expect_test_config.sanitize
    | None ->
      failwith
        (Printf.sprintf
           "Ppx_expect_runtime.read_current_test_output_exn called while there are no \
            tests running at %s"
           (Source_code_position.to_string here))
  ;;

  let am_running_expect_test = Current_test.is_running

  let default_cr_for_multiple_outputs =
    let module Configured = Configured (Expect_test_config) in
    Configured.cr_for_multiple_outputs
  ;;
end
