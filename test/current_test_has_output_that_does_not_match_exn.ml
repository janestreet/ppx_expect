[@@@alert "-ppx_expect_runtime"]

let%expect_test "not failing" =
  print_string "hello";
  [%expect {| hello |}];
  Ppx_expect_runtime.For_external.current_test_has_output_that_does_not_match_exn
    ~here:[%here]
  |> string_of_bool
  |> print_endline;
  [%expect {| false |}];
  print_string "world";
  [%expect {| world |}]
;;

let result =
  try
    Ppx_expect_runtime.For_external.current_test_has_output_that_does_not_match_exn
      ~here:[%here]
    |> string_of_bool
  with
  | exn -> Printexc.to_string exn
;;

let%expect_test "raises outside of test" =
  print_string result;
  [%expect
    {|
    ("Ppx_expect_runtime.For_external.current_test_has_output_that_does_not_match_exn called while there are no tests running"
      ppx/ppx_expect/test/current_test_has_output_that_does_not_match_exn.ml:18:12)
    |}]
;;
