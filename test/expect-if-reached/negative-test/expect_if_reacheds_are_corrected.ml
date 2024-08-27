open! Core

let%expect_test "reached and incorrect" =
  print_endline "Hello world!";
  if String.( <> ) "apples" "oranges"
  then [%expect.if_reached {| XXXXXXXXXXXX |}]
  else [%expect.unreachable]
;;
