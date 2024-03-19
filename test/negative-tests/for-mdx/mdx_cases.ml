open! Core

open struct
  module Expect_test_config = struct
    include Expect_test_config

    let upon_unreleasable_issue = `Warning_for_collector_testing
  end
end

(* $MDX part-begin=interleaved *)
let%expect_test "interleaved" =
  let l = [ "a"; "b"; "c" ] in
  printf "A list [l]\n";
  printf "It has length %d\n" (List.length l);
  [%expect {| A list [l] |}];
  List.iter l ~f:print_string;
  [%expect {|
    It has length 3
    abc
    |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=trailing *)
let%expect_test "trailing output" =
  print_endline "Hello";
  [%expect {| Hello |}];
  print_endline "world"
;;

(* $MDX part-end *)

(* $MDX part-begin=matching *)
let%expect_test "matching behavior --- no content" =
  printf "     ";
  [%expect];
  printf "     ";
  [%expect ""];
  printf "     ";
  [%expect_exact];
  printf "     ";
  [%expect_exact ""]
;;

let%expect_test "matching behavior --- one line of content" =
  printf "\n   This is one line\n\n";
  [%expect];
  printf "\n   This is one line\n\n";
  [%expect ""];
  printf "\n   This is one line\n\n";
  [%expect_exact];
  printf "\n   This is one line\n\n";
  [%expect_exact ""]
;;

let%expect_test "matching behavior --- multiple lines of content" =
  printf
    {|
Once upon a midnight dreary,
  while I pondered, weak and weary,
Over many a quaint and curious
  volume of forgotten lore |};
  [%expect];
  printf
    {|
Once upon a midnight dreary,
  while I pondered, weak and weary,
Over many a quaint and curious
  volume of forgotten lore |};
  [%expect ""];
  printf
    {|
Once upon a midnight dreary,
  while I pondered, weak and weary,
Over many a quaint and curious
  volume of forgotten lore |};
  [%expect_exact];
  printf
    {|
Once upon a midnight dreary,
  while I pondered, weak and weary,
Over many a quaint and curious
  volume of forgotten lore |};
  [%expect_exact ""]
;;

(* $MDX part-end *)

(* $MDX part-begin=bad-format *)
let%expect_test "bad formatting" =
  printf "a\n    b";
  [%expect {|
a
    b |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=exn *)
let%expect_test "exception" =
  Printexc.record_backtrace false;
  printf "start!";
  [%expect {| |}];
  let sum = 2 + 2 in
  if sum <> 3
  then (
    printf "%d" sum;
    failwith "nope");
  printf "done!";
  [%expect {| done! |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=function *)
let%expect_test "function" =
  let f output =
    print_string output;
    [%expect {| hello world |}]
  in
  f "hello world";
  f "hello world"
;;

(* $MDX part-end *)

(* $MDX part-begin=broken-function *)
let%expect_test "function" =
  let f output =
    print_string output;
    [%expect {| hello world |}]
  in
  f "hello world";
  f "goodbye world";
  f "once upon\na midnight dreary";
  f "hello world"
;;

(* $MDX part-end *)

(* $MDX part-begin=unreachable *)
let%expect_test "unreachable" =
  let interesting_bool = 3 > 5 in
  printf "%b\n" interesting_bool;
  if interesting_bool
  then [%expect {| true |}]
  else (
    printf "don't reach\n";
    [%expect.unreachable])
;;

(* $MDX part-end *)

(* $MDX part-begin=sometimes-reachable *)
module Test (B : sig
  val interesting_opt : int option
end) =
struct
  let%expect_test "sometimes reachable" =
    match B.interesting_opt with
    | Some x ->
      printf "%d\n" x;
      [%expect {| 5 |}]
    | None -> [%expect {| |}]
  ;;
end

module _ = Test (struct
  let interesting_opt = Some 5
end)

module _ = Test (struct
  let interesting_opt = None
end)

module _ = Test (struct
  let interesting_opt = Some 5
end)

(* $MDX part-end *)

(* $MDX part-begin=sometimes-raises *)
module Test' (B : sig
  val interesting_opt : int option
end) =
struct
  let%expect_test "sometimes raises" =
    match B.interesting_opt with
    | Some x ->
      printf "%d\n" x;
      [%expect {| 5 |}]
    | None -> failwith "got none!"
  ;;
end

module _ = Test' (struct
  let interesting_opt = Some 5
end)

module _ = Test' (struct
  let interesting_opt = None
end)

module _ = Test' (struct
  let interesting_opt = Some 5
end)

(* $MDX part-end *)

(* $MDX part-begin=output-capture *)

(* Suppose we want to test code that attaches a timestamp to everything it prints *)
let print_message s = printf "%s: %s\n" (Time_float.to_string_utc (Time_float.now ())) s

let%expect_test "output capture" =
  (* A simple way to clean up the non-determinism is to 'X' all digits *)
  let censor_digits s = String.map s ~f:(fun c -> if Char.is_digit c then 'X' else c) in
  print_message "Hello";
  [%expect.output] |> censor_digits |> print_endline;
  [%expect {| |}];
  print_message "world";
  [%expect.output] |> censor_digits |> print_endline;
  [%expect {| |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=sanitization *)

(* Suppose we want to test code that attaches a timestamp to everything it prints *)
let print_message s = printf "%s: %s\n" (Time_float.to_string_utc (Time_float.now ())) s

module Expect_test_config = struct
  include Expect_test_config

  (* A simple way to clean up the non-determinism is to 'X' all digits *)
  let sanitize s = String.map s ~f:(fun c -> if Char.is_digit c then 'X' else c)
end

let%expect_test "sanitization" =
  print_message "Hello";
  [%expect {| |}];
  print_message "world";
  [%expect {| |}]
;;

(* $MDX part-end *)
