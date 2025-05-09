module Expectation = Ppx_expect_runtime.For_external.Expectation
[@@alert "-ppx_expect_runtime"]

let%expect_test "not committed or skipped" =
  [%expectation "foo"];
  ()
;;

let%expect_test "raise before committing" =
  print_endline "foo";
  [%expectation "foo"];
  assert (1 = 2);
  Expectation.commit ()
;;

let%expect_test "commited while not active" =
  Expectation.commit ();
  ()
;;

let%expect_test "skipped while not active" =
  Expectation.skip ();
  ()
;;

let%expect_test "commited, then skipped" =
  let do_expectation () = [%expectation {| |}] in
  print_endline "ok";
  do_expectation ();
  Expectation.commit ();
  print_endline "not_ok";
  do_expectation ();
  let collected = Expectation.actual () in
  Expectation.skip ();
  [%expect {| |}];
  print_endline collected;
  [%expect {| |}]
;;

let%expect_test "multiple expectation" =
  print_endline "foo";
  [%expectation "foo"];
  [%expectation "bar"];
  Expectation.commit ()
;;

let%expect_test "expect before committed" =
  print_endline "foo";
  [%expectation "foo"];
  [%expect "foo"];
  Expectation.commit ()
;;

let%expect_test "expectation fails and never committed" =
  Printf.printf "foo";
  [%expectation "bar"];
  let is_successful, expected, actual =
    Expectation.is_successful (), Expectation.expected (), Expectation.actual ()
  in
  Expectation.commit ();
  print_endline (Bool.to_string is_successful);
  [%expect {| false |}];
  print_endline (Option.value ~default:"<NONE>" expected);
  [%expect {| bar |}];
  print_endline actual;
  [%expect {| foo |}]
;;

let%expect_test "skipped" =
  let () = [%expectation {| hello world |}] in
  Expectation.skip ()
;;

let%expect_test "unreached" =
  let _ = fun () -> [%expectation {| hello world |}] in
  [%expect {| |}]
;;

let%expect_test "unreachable is committed" =
  Printf.printf "foo";
  [%expectation.never_committed];
  let is_successful, expected, actual =
    Expectation.is_successful (), Expectation.expected (), Expectation.actual ()
  in
  Expectation.commit ();
  print_endline (Bool.to_string is_successful);
  [%expect {| false |}];
  print_endline (Option.value ~default:"<NONE>" expected);
  [%expect {| <NONE> |}];
  print_endline actual;
  [%expect {| foo |}]
;;

let%expect_test "expectation fails and corrected" =
  Printf.printf "foo";
  [%expectation "bar"];
  Expectation.commit ()
;;

let%expect_test "expectation committed twice" =
  Printf.printf "foo";
  [%expectation "bar"];
  Expectation.commit ();
  Expectation.commit ()
;;

let%expect_test "multiline and indentation mismatches" =
  Printf.printf "line 1\n";
  Printf.printf " line 2\n";
  Printf.printf "  line 3\n";
  [%expectation
    {|
      line 1
      line 2
      line 3
      |}];
  let is_successful, expected, actual =
    Expectation.is_successful (), Expectation.expected (), Expectation.actual ()
  in
  Expectation.commit ();
  print_endline (Bool.to_string is_successful);
  [%expect {| false |}];
  print_endline (Option.value ~default:"<NONE>" expected);
  [%expect
    {|
    line 1
    line 2
    line 3
    |}];
  print_endline actual;
  [%expect
    {|
    line 1
     line 2
      line 3
    |}]
;;
