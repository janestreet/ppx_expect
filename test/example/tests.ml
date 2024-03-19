open Core

(* We may use other syntax extensions when writing expect tests. *)
type t = int list [@@deriving sexp_of]

let pr s = Printf.printf "%s\n" s

let%expect_test "foo" =
  pr "line1";
  pr (Sexp.to_string (sexp_of_t [ 1; 2; 3 ]));
  [%expect {|
    line1
    (1 2 3)
    |}]
;;

let%expect_test _ =
  print_string "hello, world!";
  [%expect "hello, world!"]
;;

let%expect_test _ =
  print_string "hello, world!";
  [%expect_exact {|hello, world!|}]
;;

let%expect_test _ =
  print_string "I need |}weird escaping";
  [%expect {xxx| I need |}weird escaping |xxx}]
;;
