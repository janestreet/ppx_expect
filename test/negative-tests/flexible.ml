open! Core

(*
   In old versions of [ppx_expect], the below tests would respect the formatting of the
   incorrect output present when possible.

   Currently, [ppx_expect] instead enforces standardized formatting in [[%expect]] nodes,
   so we instead use this test to demonstrate that all of the below expectations are
   reformatted to the same thing.
*)

(* Single line actual... *)

let%expect_test _ =
  print_string "hello";
  [%expect {||}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|
  |}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|
           |}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|  WRONG
           |}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|  WRONG
           |}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|
  WRONG |}]
;;

let%expect_test _ =
  print_string "hello";
  [%expect {|
       WRONG
  |}]
;;

(* Multi line actual... *)

let%expect_test _ =
  print_string "one1\ntwo";
  [%expect {||}]
;;

let%expect_test _ =
  print_string "one2\ntwo";
  [%expect {|
  |}]
;;

let%expect_test _ =
  print_string "one3\ntwo";
  [%expect {|
           |}]
;;

let%expect_test _ =
  print_string "one4\ntwo";
  [%expect {|  WRONG
           |}]
;;

let%expect_test _ =
  print_string "one5\ntwo";
  [%expect {|
  WRONG |}]
;;

let%expect_test _ =
  print_string "one6\ntwo";
  [%expect {|
       WRONG
  |}]
;;

let%expect_test _ =
  print_string "one8\ntwo";
  [%expect {|
  WRONG
  THING |}]
;;

let%expect_test _ =
  print_string "one9\ntwo";
  [%expect {|
       WRONG
       THING
  |}]
;;

let%expect_test _ =
  print_string "one10\ntwo";
  [%expect {|
       WRONG
          THING
  |}]
;;

let%expect_test _ =
  print_string "one11\ntwo";
  [%expect {|
          WRONG
       THING
  |}]
;;
