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

let%expect_test "failing" =
  print_string "hello";
  [%expect {| goodbye |}];
  Ppx_expect_runtime.For_external.current_test_has_output_that_does_not_match_exn
    ~here:[%here]
  |> string_of_bool
  |> print_endline;
  [%expect {| |}];
  print_string "world";
  [%expect {| world |}]
;;

let%expect_test "not failing 2 --- 'failure state' is not preserved across tests" =
  print_string "hello";
  [%expect {| hello |}];
  Ppx_expect_runtime.For_external.current_test_has_output_that_does_not_match_exn
    ~here:[%here]
  |> string_of_bool
  |> print_endline;
  [%expect {| false |}];
  print_string "world";
  [%expect {| |}]
;;
