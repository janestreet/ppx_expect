open! Base
open Ppx_expect_runtime_types [@@alert "-ppx_expect_runtime_types"]

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

module Expect_node_formatting = Expect_node_formatting
module Compact_loc = Compact_loc
module Expectation_id = Expectation_id
module Delimiter = String_node_format.Delimiter
module Payload = Payload
module Current_file = Current_file
module Test_node = Test_node
module Write_corrected_file = Write_corrected_file
module Make_test_block = Test_block.Make
module For_external = Test_block.For_external
module For_apply_style = Test_spec.For_apply_style
