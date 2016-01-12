open Ppx_inline_test_lib.Runtime

(** Evaluate the results of all the tests run through Expect_test_runner. *)
val evaluate_tests : use_color:bool -> Test_result.t
