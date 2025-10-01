let%expect_test _ =
  ignore
    (Ppx_expect_runtime.For_external.push_output_exn ~here:[%here]
     : Ppx_expect_runtime.For_external.Stack_frame.t) [@alert "-ppx_expect_runtime"]
;;
