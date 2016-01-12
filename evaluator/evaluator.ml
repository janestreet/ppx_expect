let () =
  Ppx_inline_test_lib.Runtime.Test_result.record
    (Expect_test_evaluator_lib.Evaluator.evaluate_tests
       ~use_color:(Ppx_inline_test_lib.Runtime.use_color))
