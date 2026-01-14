open! Base
open! Portable
open Ppx_expect_runtime_types [@@alert "-ppx_expect_runtime_types"]
open Capsule.Blocking_sync [@@alert "-deprecated"]

let sexp_of_character_range ({ start_pos; end_pos; _ } : Compact_loc.t) : Sexp.t =
  List
    [ List [ Atom "start_pos"; sexp_of_int start_pos ]
    ; List [ Atom "end_pos"; sexp_of_int end_pos ]
    ]
;;

(* [Expectation] stores information about a particular encounter with an [[%expectation]]
   node. *)

module Expectation = struct
  type t =
    { actual : string
    ; expected : string option
    ; is_successful : bool
    ; character_range : Compact_loc.t
    ; commit : unit -> unit
    }

  let sexp_of_t { actual; expected; is_successful; character_range; commit = _ } : Sexp.t =
    List
      [ List [ Atom "actual"; sexp_of_string actual ]
      ; List [ Atom "expected"; sexp_of_option sexp_of_string expected ]
      ; List [ Atom "is_successful"; sexp_of_bool is_successful ]
      ; List [ Atom "character_range"; sexp_of_character_range character_range ]
      ]
  ;;
end

module Match_or_mismatch = struct
  type t =
    | Match
    | Mismatch
end

(** [Shared] and [Configured] primarily contain boilerplate involving the FFI and printing
    [CR]s. The interesting logic is in [Make]. *)

module Shared : sig
  type t

  val src_filename : t -> string
  val output_file : t -> string
  val failure_atomic : t -> bool Atomic.t
  val expectation_atomic : t -> Expectation.t option Atomic.t
  val set_up_block : string -> t
  val read_test_output_unsanitized : t -> string
  val flush : unit -> unit
  val clean_up_block : t -> unit
  val assert_no_expectation : message:(Sexp.t -> Sexp.t) -> t -> unit
  val assert_stack_empty : message:(Sexp.t -> Sexp.t) -> t -> unit

  module Stack_frame : sig
    type t
  end

  val push_output_onto_stack : t -> Stack_frame.t
  val pop_output_from_stack : t -> Stack_frame.t -> Match_or_mismatch.t
end = struct
  (** Simple data structure for strings with fast append *)
  module Rope : sig
    type t

    val empty : t
    val append : t -> t -> t
    val of_string : string -> t
    val to_string : t -> string
  end = struct
    type t =
      | One of string
      | Two of t * t

    let empty = One ""
    let of_string string = One string

    let append a b =
      match a, b with
      | One "", t | t, One "" -> t
      | a, b -> Two (a, b)
    ;;

    let rec to_list_loop ~rev_ropes ~strings =
      match rev_ropes with
      | [] -> strings
      | One string :: rev_ropes -> to_list_loop ~rev_ropes ~strings:(string :: strings)
      | Two (a, b) :: rev_ropes -> to_list_loop ~rev_ropes:(b :: a :: rev_ropes) ~strings
    ;;

    let to_list t = to_list_loop ~rev_ropes:[ t ] ~strings:[]
    let to_string t = String.concat (to_list t) [@nontail]
  end

  type%fuelproof 'k inner =
    { src_filename : string
    ; output_file : string
    ; fail : bool Atomic.t
    ; expectation : Expectation.t option Atomic.t
    ; test_output_reader : Stdlib.in_channel
    ; test_output_writer : Stdlib.out_channel
    ; output_reader_for_stubs_do_not_read : Stdlib.in_channel option
    ; (* The following three fields are mutated together, so are synchronized using the
         same mutex *)
      old_offset : (int ref, 'k) Capsule.Data.t
    ; pushed_output : (Rope.t Stack.t, 'k) Capsule.Data.t
    ; popped_output : (Rope.t ref, 'k) Capsule.Data.t
    ; mutex : 'k Mutex.t
    }

  type%fuelproof t = T : 'k inner -> t

  let src_filename (T { src_filename; _ }) = src_filename
  let output_file (T { output_file; _ }) = output_file
  let failure_atomic (T { fail; _ }) = fail
  let expectation_atomic (T { expectation; _ }) = expectation

  external redirect_stdout
    :  output_writer:Stdlib.out_channel
    -> output_reader_for_stubs_do_not_read:Stdlib.in_channel option
    -> stdout:Stdlib.out_channel
    -> stderr:Stdlib.out_channel
    -> unit
    = "ppx_expect_runtime_before_test"

  external restore_stdout
    :  stdout:Stdlib.out_channel
    -> stderr:Stdlib.out_channel
    -> unit
    = "ppx_expect_runtime_after_test"

  (* SAFETY: These two functions are not in general thread safe, but are only ever called
     from one thread at a time. This is ensured because of the synchronization on
     [Current_test.current_test] *)
  let redirect_stdout = Obj.magic_portable redirect_stdout
  let restore_stdout = Obj.magic_portable restore_stdout

  external pos_out : Stdlib.out_channel -> int = "ppx_expect_runtime_out_channel_position"
  external flush_stubs : unit -> unit = "ppx_expect_runtime_flush_stubs_streams"

  (** Save std file descriptors, open a temp file for test output, and reroute stdout and
      stderr there. *)
  let set_up_block src_filename =
    let output_file =
      Current_file.absolute_path
        ~filename_rel_to_cwd:(Stdlib.Filename.temp_file "expect-test" "output")
    in
    let test_output_writer =
      Stdlib.open_out_gen [ Open_wronly; Open_creat; Open_binary ] 0o644 output_file
    in
    let test_output_reader = Stdlib.open_in_bin output_file in
    let output_reader_for_stubs_do_not_read =
      Option.some_if_thunk (Ppx_inline_test_lib.verbose ()) (fun () ->
        Stdlib.open_in_bin output_file)
    in
    redirect_stdout
      ~output_writer:test_output_writer
      ~output_reader_for_stubs_do_not_read
      ~stdout:Stdlib.stdout
      ~stderr:Stdlib.stderr;
    let (P key) = Capsule.Expert.create () in
    let mutex = Mutex.create key in
    T
      { src_filename
      ; output_file
      ; test_output_reader
      ; test_output_writer
      ; output_reader_for_stubs_do_not_read
      ; old_offset = Capsule.Data.create (fun () -> ref 0)
      ; fail = Atomic.make false
      ; expectation = Atomic.make None
      ; pushed_output = Capsule.Data.create Stack.create
      ; popped_output = Capsule.Data.create (fun () -> ref Rope.empty)
      ; mutex
      }
  ;;

  (** Close the temp file and restore stdout and stderr. *)
  let clean_up_block
    (T
      { output_file
      ; test_output_reader
      ; test_output_writer
      ; output_reader_for_stubs_do_not_read
      ; src_filename = _
      ; fail = _
      ; expectation = _
      ; old_offset = _
      ; pushed_output = _
      ; popped_output = _
      ; mutex = _
      })
    =
    Stdlib.close_in test_output_reader;
    Stdlib.close_out test_output_writer;
    restore_stdout ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    Option.iter output_reader_for_stubs_do_not_read ~f:Stdlib.close_in;
    Stdlib.Sys.remove output_file
  ;;

  let get_std_formatter = Obj.magic_portable Stdlib.Format.get_std_formatter
  let get_err_formatter = Obj.magic_portable Stdlib.Format.get_err_formatter

  let flush () =
    Stdlib.Format.pp_print_flush (get_std_formatter ()) ();
    Stdlib.Format.pp_print_flush (get_err_formatter ()) ();
    Stdlib.flush Stdlib.stdout;
    Stdlib.flush Stdlib.stderr;
    flush_stubs ()
  ;;

  let read_test_output_unsanitized_rope
    ~access
    { test_output_reader; old_offset; popped_output; _ }
    =
    let old_offset = Capsule.Data.unwrap ~access old_offset in
    let popped_output = Capsule.Data.unwrap ~access popped_output in
    let new_offset =
      flush ();
      pos_out Stdlib.stdout
    in
    let len = new_offset - !old_offset in
    old_offset := new_offset;
    let old_output = !popped_output in
    let new_output = Rope.of_string (Stdlib.really_input_string test_output_reader len) in
    popped_output := Rope.empty;
    Rope.append old_output new_output
  ;;

  let read_test_output_unsanitized (T t) =
    (Mutex.with_lock t.mutex ~f:(fun password ->
       Capsule.Expert.access ~password ~f:(fun access ->
         { aliased = Rope.to_string (read_test_output_unsanitized_rope ~access t) })
       [@nontail]))
      .aliased
  ;;

  module Stack_frame = struct
    (* We use the length of the stack after a frame is pushed as an ID for the frame. *)
    type t = int
  end

  let push_output_onto_stack (T t) =
    Mutex.with_lock t.mutex ~f:(fun password ->
      Capsule.Expert.access ~password ~f:(fun access ->
        let output = read_test_output_unsanitized_rope ~access t in
        let stack = Capsule.Data.unwrap ~access t.pushed_output in
        Stack.push stack output;
        Stack.length stack)
      [@nontail])
    [@nontail]
  ;;

  let pop_output_from_stack (T { pushed_output; popped_output; mutex; _ }) stack_frame =
    Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Expert.access ~password ~f:(fun access ->
        let pushed_output = Capsule.Data.unwrap ~access pushed_output in
        let popped_output = Capsule.Data.unwrap ~access popped_output in
        let matched : Match_or_mismatch.t =
          if Stack.length pushed_output = stack_frame then Match else Mismatch
        in
        (* If we're popping out of order, pop down to the current frame. This results in
           more sensible errors than just refusing to pop anything. We'll be checking at
           every other [pop_output_from_stack], and at end of the expect test, anyway. *)
        while Stack.length pushed_output >= stack_frame do
          let pushed = Stack.pop_exn pushed_output in
          Ref.replace popped_output (fun popped -> Rope.append pushed popped)
        done;
        matched)
      [@nontail])
    [@nontail]
  ;;

  let assert_no_expectation ~message (T { expectation; _ }) =
    match Atomic.get expectation with
    | Some e -> e |> Expectation.sexp_of_t |> message |> raise_s
    | None -> ()
  ;;

  let assert_stack_empty ~message (T { mutex; pushed_output; _ }) =
    match
      (Mutex.with_lock mutex ~f:(fun password ->
         Capsule.Expert.access ~password ~f:(fun access ->
           let stack = Capsule.Data.unwrap ~access pushed_output in
           { aliased = Stack.to_list stack })
         [@nontail]))
        .aliased
    with
    | [] -> ()
    | nonempty ->
      nonempty
      |> List.map ~f:Rope.to_string
      |> sexp_of_list sexp_of_string
      |> message
      |> raise_s
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
    match possible_exn () with
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

  let run_and_dump_backtrace possible_exn = dump_backtrace (fun () -> C.run possible_exn)
end

(** The expect test currently being executed and some info we print if the program crashes
    in the middle of a test. *)
module Current_test : sig
  module Or_no_test_running : sig
    type 'a t =
      | Ok of 'a
      | No_test_running
  end

  type t =
    { line_number : int
    ; basename : string
    ; location : Compact_loc.t
    ; test_block : Shared.t
    ; test_name : string option
    }

  val set : t -> unit
  val unset : unit -> unit
  val is_running : unit -> bool
  val current_test : unit -> Shared.t Or_no_test_running.t
  val current_test_exn : unit -> Shared.t
  val current_test_name : unit -> string option Or_no_test_running.t
  val iter : f:(t -> unit) -> unit
  val assert_no_test_running : basename:string -> line_number:int -> unit
end = struct
  module Or_no_test_running = struct
    type 'a t =
      | Ok of 'a
      | No_test_running

    let map t ~f =
      match t with
      | Ok a -> Ok (f a)
      | No_test_running -> No_test_running
    ;;
  end

  type t =
    { line_number : int
    ; basename : string
    ; location : Compact_loc.t
    ; test_block : Shared.t
    ; test_name : string option
    }

  let test_is_running =
    (* NOTE: This is an atomic, not a dynamic, because we guarantee that only one test is
       running at once *)
    Atomic.make (No_test_running : t Or_no_test_running.t)
  ;;

  let set t = Atomic.set test_is_running (Ok t)
  let unset () = Atomic.set test_is_running No_test_running
  let get () = Atomic.get test_is_running

  let is_running () =
    match get () with
    | Ok _ -> true
    | No_test_running -> false
  ;;

  let current_test () =
    Or_no_test_running.map (get ()) ~f:(fun { test_block; _ } -> test_block)
  ;;

  let current_test_exn () =
    match current_test () with
    | Ok x -> x
    | No_test_running ->
      failwith
        "Internal expect_test error: [current_test_exn] called when no test running"
  ;;

  let current_test_name () =
    Or_no_test_running.map (get ()) ~f:(fun { test_name; _ } -> test_name)
  ;;

  let iter ~f =
    match get () with
    | Ok a -> f a
    | No_test_running -> ()
  ;;

  let assert_no_test_running ~basename ~line_number =
    iter
      ~f:
        (fun
          { line_number = outer_line_number
          ; basename = outer_basename
          ; location = _
          ; test_block = _
          ; test_name = _
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

(** The main testing functions of a test block, which depend on configurations. *)
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

  let run_test_general ~skip_expectation_check ~test_id ~test_output_raw t =
    let node =
      Test_node.Global_results_table.find_test
        ~absolute_filename:(Shared.src_filename t)
        ~test_id
    in
    if not skip_expectation_check
    then
      Shared.assert_no_expectation
        ~message:(fun e ->
          Sexp.message
            "reached an expect node with unresolved [[%expectation]]"
            [ "expectation", e
            ; "character_range", sexp_of_character_range (Test_node.loc node)
            ])
        t;
    let result, _ =
      Test_node.compute_but_do_not_record_test_result
        ~expect_node_formatting:Expect_node_formatting.default
        ~test_output_raw
        node
    in
    ( node
    , result
    , fun () ->
        Test_node.record_result
          ~test_output_raw
          ~failure_atomic:(Shared.failure_atomic t)
          node
          result;
        Atomic.set (Shared.expectation_atomic t) None )
  ;;

  let run_test_inner ~skip_expectation_check ~test_id ~test_output_raw t =
    let _node, _result, commit =
      run_test_general ~skip_expectation_check ~test_id ~test_output_raw t
    in
    commit ()
  ;;

  let run_test ~test_id =
    Current_test.current_test_exn ()
    |> run_test_inner
         ~skip_expectation_check:false
         ~test_id
         ~test_output_raw:(read_test_output_sanitized_and_checked ())
  ;;

  let run_test_without_commiting ~test_id =
    let t = Current_test.current_test_exn () in
    let test_output_raw = read_test_output_sanitized_and_checked () in
    let node, result, commit =
      run_test_general ~skip_expectation_check:false ~test_id ~test_output_raw t
    in
    let is_successful =
      match result with
      | Pass -> true
      | Fail _ -> false
    in
    Atomic.set
      (Shared.expectation_atomic t)
      (Some
         { actual = test_output_raw
         ; expected = Test_node.expectation_of_t node
         ; is_successful
         ; character_range = Test_node.loc node
         ; commit
         })
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
        let absolute_filename =
          Current_file.absolute_path ~filename_rel_to_cwd:basename
        in
        (* Create the tests for trailing output and uncaught exceptions *)
        let expectations =
          let trailing_test =
            Test_spec.expect_trailing
              ~insert_loc:
                { loc = { trailing_loc with end_pos = trailing_loc.start_pos }; body_loc }
            |> Test_node.of_expectation
          in
          let exn_test =
            match expected_exn with
            | Some _ ->
              Test_spec.expect_uncaught_exn
                ~formatting_flexibility
                ~located_payload:expected_exn
                ~node_loc:trailing_loc
              |> Test_node.of_expectation
            | None ->
              Test_spec.expect_no_uncaught_exn
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
        Current_test.set
          { line_number; basename; location; test_block; test_name = description };
        let test_exn =
          let body_exn =
            Configured.run_and_dump_backtrace (fun () ->
              (* Ignore output that was printed before the test started *)
              let (_ : string) = Shared.read_test_output_unsanitized test_block in
              (* Run the test body *)
              f ())
          in
          let open_expectation_exn =
            Configured.dump_backtrace (fun () ->
              (* Assert all [expectation]s are closed by the end of the test *)
              Shared.assert_no_expectation test_block ~message:(fun e ->
                Sexp.message
                  "reached end of test with unresolved [[%expectation]]"
                  [ "expectation", e ]))
          in
          let stack_empty_exn =
            Configured.dump_backtrace (fun () ->
              Shared.assert_stack_empty test_block ~message:(fun s ->
                Sexp.message
                  "reached end of test before completion of \
                   [with_empty_expect_test_output_async] or similar function"
                  [ "stack", s ]))
          in
          [ body_exn; open_expectation_exn; stack_empty_exn ]
          |> List.filter_opt
          |> function
          | [] -> None
          | exns -> Some (String.concat ~sep:"\n" exns)
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
        run_test_inner
          test_block
          ~skip_expectation_check:true
            (* We have left the part of the test that catches exceptions, and so do not
               wish to re-raise if the test raised without finishing an [[%expectation]]
            *)
          ~test_output_raw:test_output
          ~test_id:test_to_run;
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
        ; test_name = _
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
  let require_test_running
    ~here
    ~function_name
    (or_no_test_running : _ Current_test.Or_no_test_running.t)
    =
    match or_no_test_running with
    | Ok a -> a
    | No_test_running ->
      raise_s
        (Sexp.message
           (Printf.sprintf
              "Ppx_expect_runtime.For_external.%s called while there are no tests running"
              function_name)
           [ "", Source_code_position.sexp_of_t here ])
  ;;

  let read_current_test_output_exn ~here =
    Current_test.current_test ()
    |> require_test_running ~here ~function_name:"read_current_test_output_exn"
    |> Shared.read_test_output_unsanitized
    |> Expect_test_config.sanitize
  ;;

  module Stack_frame = Shared.Stack_frame
  module Match_or_mismatch = Match_or_mismatch

  let push_output_exn ~here =
    let shared =
      Current_test.current_test ()
      |> require_test_running ~here ~function_name:"push_output_exn"
    in
    Shared.push_output_onto_stack shared
  ;;

  let pop_output_exn ~here frame =
    let shared =
      Current_test.current_test ()
      |> require_test_running ~here ~function_name:"pop_output_exn"
    in
    Shared.pop_output_from_stack shared frame
  ;;

  let am_running_expect_test = Current_test.is_running

  let current_expect_test_name_exn ~here =
    Current_test.current_test_name ()
    |> require_test_running ~here ~function_name:"current_expect_test_name_exn"
  ;;

  let default_cr_for_multiple_outputs =
    let module Configured = Configured (Expect_test_config) in
    Configured.cr_for_multiple_outputs
  ;;

  let current_test_has_output_that_does_not_match_exn ~here =
    Current_test.current_test ()
    |> require_test_running
         ~here
         ~function_name:"current_test_has_output_that_does_not_match_exn"
    |> Shared.failure_atomic
    |> Atomic.get
  ;;

  let with_current ~here ~function_name ~f =
    Current_test.current_test () |> require_test_running ~here ~function_name |> f
  ;;

  module Expectation = struct
    let is_active ?(here = Stdlib.Lexing.dummy_pos) () =
      with_current
        ~f:(fun t -> Option.is_some (Atomic.get (Shared.expectation_atomic t)))
        ~here
        ~function_name:"is_active"
    ;;

    let get_exn ~function_name ~here t =
      match Atomic.get (Shared.expectation_atomic t) with
      | None ->
        raise_s
          (Sexp.message
             (Printf.sprintf
                "Ppx_expect_runtime.For_external.Expectation.%s called with no \
                 unresolved [[%%expectation]]"
                function_name)
             [ "", Source_code_position.sexp_of_t here ])
      | Some expectation -> expectation
    ;;

    let run_over_expectation ~here ~function_name ~f =
      with_current ~here ~function_name:("Expectation." ^ function_name) ~f:(fun t ->
        f t (get_exn ~here ~function_name t))
    ;;

    let commit ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"commit" ~f:(fun _ e -> e.commit ())
    ;;

    let skip ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"skip" ~f:(fun t (_ : Expectation.t) ->
        Atomic.set (Shared.expectation_atomic t) None)
    ;;

    let sexp_for_debugging ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"sexp_for_debugging" ~f:(fun _ e ->
        Expectation.sexp_of_t e)
    ;;

    let is_successful ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"is_successful" ~f:(fun _ e ->
        e.is_successful)
    ;;

    let actual ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"actual" ~f:(fun _ e -> e.actual)
    ;;

    let expected ?(here = Stdlib.Lexing.dummy_pos) () =
      run_over_expectation ~here ~function_name:"expected" ~f:(fun _ e -> e.expected)
    ;;
  end
end
