[%%duplicate
  let%expect_test "apples and apples" =
    print_endline "buy apple";
    [%expect {| buy apple |}]
  ;;]
