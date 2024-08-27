open! Core

let%expect_test "unreached" =
  print_endline "Hello world!";
  if String.( = ) "apples" "oranges"
  then [%expect.if_reached {| XXXXXXXXXXXX |}]
  else [%expect {| Hello world! |}]
;;

let%expect_test "reached and correct" =
  print_endline "Hello world!";
  if String.( = ) "buffalo" "buffalo"
  then [%expect.if_reached {| Hello world! |}]
  else [%expect.unreachable]
;;

module F (Arg : sig
    val x : int
  end) =
struct
  let%expect_test "sometimes reached within one proc" =
    print_endline "Hello world!";
    if Arg.x = 0
    then [%expect.if_reached {| Hello world! |}]
    else [%expect {| Hello world! |}]
  ;;
end

module _ = F (struct
    let x = 0
  end)

module _ = F (struct
    let x = 1
  end)
