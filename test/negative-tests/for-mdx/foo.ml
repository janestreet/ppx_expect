(* $MDX part-begin=addition *)
open! Core

let%expect_test "addition" =
  printf "%d" (1 + 2);
  [%expect {| 4 |}]
;;

(* $MDX part-end *)
