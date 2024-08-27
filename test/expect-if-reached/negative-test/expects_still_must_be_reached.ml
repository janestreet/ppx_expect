open! Core

let%expect_test "a normal expect is unreached" =
  print_endline "Hello world!";
  if String.( <> ) "apples" "oranges"
  then [%expect.if_reached {| Hello world! |}]
  else [%expect {| XXXXXXXXXXXX |}]
;;
