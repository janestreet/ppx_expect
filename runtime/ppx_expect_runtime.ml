open! Base

(** This library provides the runtime representation of expect tests and much of the logic
    for running them.

    The [Test_block] module defines the runtime representation of the whole
    [let%expect_test] block. It exports a [Make] functor that is used in generated code to
    produce a module from the locally bound [Expect_test_config]. [run_suite] from the
    resulting module takes in remaining information about the expect test, including
    inline test configurations, information about the contained expectations, and a
    callback containing the body of the test.

    The [~expectations] argument to [run_suite] is an assoc list mapping unique ids to
    [Test_node.t]s. A [Test_node.t] stores the representation of a single [[%expect]] test
    AST node and collects the results of tests that reach this node.

    In the body of the test, the [[%expect]] AST nodes are replaced by calls to
    [run_test], with the appropriate id passed as an argument.

    For an example, consider a file that contains just the simple [let%expect_test] below:

    {[
      let%expect_test _ =
        print_string "Hello";
        [%expect {| Hello |}];
        print_string "world";
        [%expect_exact {x|world|x}]
      ;;
    ]}

    It will expand to code that looks something like this:

    {[
      (* This statement is added to the top of each rewritten file; it is used to make
         sure tests are only run from the files in which they are declared. *)
      let () =
        Ppx_expect_runtime.Current_file.set
          ~filename_rel_to_project_root:"foo/bar/test/test.ml"

      (* Each test expands into something that looks approximately like this. Some of the
         arguments to [Ppx_expect_test_block.run_suite] are elided for clarity. *)
      let () =
        (* Prepare to read test output using the settings from [Expect_test_config] *)
        let module Ppx_expect_test_block =
          Ppx_expect_runtime.Make_test_block(Expect_test_config) in
        Ppx_expect_test_block.run_suite
          (* The name of the file in which the test is defined. This lets the runtime
             check that the filename set here at ppx-time matches the one that is set by
             the block above at runtime. If the test were defined in a functor and that
             functor invoked from another file, the filenames would not match. *)
          ~filename_rel_to_project_root:"foo/bar/test/test.ml"
          (* The ids that should be used when registering the trailing output test and the
             uncaught exception tests. They are minted at ppx time because that is the
             time that it is easiest to guarantee their uniqueness. *)
          ~trailing_test_id:(Ppx_expect_runtime.Expectation_id.of_int 2)
          ~exn_test_id:(Ppx_expect_runtime.Expectation_id.of_int 3)
          (* An assoc list mapping ids to representations of expect nodes that appear in
             this test. Later, when encountering expect nodes, information about them is
             looked up in this table. *)
          ~expectations:(([(Ppx_expect_runtime.Expectation_id.of_int 1,
                            Ppx_expect_runtime.Test_node.Create.expect_exact
                              { contents = "world"; tag = (Tag "x") }
                              { start_bol = ...; start_pos = ...; end_pos = ... });
                           (Ppx_expect_runtime.Expectation_id.of_int 0,
                            Ppx_expect_runtime.Test_node.Create.expect
                              { contents = " Hello "; tag = (Tag "") }
                              { start_bol = ...; start_pos = ...; end_pos = ... })])
                        )
          (* The body of the let binding is passed as a callback. *)
          (fun () ->
             print_string "Hello";
             (* Tests are run by passing in the id of the encountered test node. *)
             Ppx_expect_test_block.run_test
               ~test_id:(Ppx_expect_runtime.Expectation_id.of_int 0);
             print_string "world";
             Ppx_expect_test_block.run_test
               ~test_id:(Ppx_expect_runtime.Expectation_id.of_int 1))

      (* This statement is added to the end of each file so that the expect test runtime
         knows the file is finished executing and a new one can be set as current. *)
      let () = Ppx_expect_runtime.Current_file.unset ()
    ]}
*)

(* Register the reachability check and corrected file writing as an evaluator with
   [Ppx_inline_test_lib] *)
let () =
  Ppx_inline_test_lib.add_evaluator ~f:(fun () ->
    Stdlib.Sys.chdir (Lazy.force Current_file.initial_dir);
    Test_node.Global_results_table.process_each_file
      ~f:(fun ~filename ~test_nodes ~postprocess ->
      Write_corrected_file.f
        test_nodes
        ~use_color:(Ppx_inline_test_lib.use_color ())
        ~in_place:(Ppx_inline_test_lib.in_place ())
        ~diff_command:(Ppx_inline_test_lib.diff_command ())
        ~diff_path_prefix:(Ppx_inline_test_lib.diff_path_prefix ())
        ~with_:postprocess
        ~filename)
    |> Ppx_inline_test_lib.Test_result.combine_all)
;;

(* Alert of mid-test runtime failure. *)
let () = Stdlib.at_exit Test_block.at_exit

(* Exported definitions *)

module Expect_node_formatting = Types.Expect_node_formatting
module Compact_loc = Types.Compact_loc
module Expectation_id = Types.Expectation_id
module Delimiter = Types.String_node_format.Delimiter
module Payload = Output.Payload

module Current_file : sig
  val set : filename_rel_to_project_root:string -> unit
  val unset : unit -> unit
end =
  Current_file

module Test_node = struct
  type t = Test_node.t

  module Create = Test_node.Create
  module For_mlt = Test_node.For_mlt
end

module Write_corrected_file = Write_corrected_file
module Make_test_block = Test_block.Make
module For_external = Test_block.For_external
module For_apply_style = Expectation.For_apply_style
