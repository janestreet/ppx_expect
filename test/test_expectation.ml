module Expectation = Ppx_expect_runtime.For_external.Expectation
[@@alert "-ppx_expect_runtime"]

let%expect_test "expectation passes" =
  Printf.printf "foo";
  [%expectation "foo"];
  Expectation.sexp_for_debugging () |> Sexplib0.Sexp.to_string_hum |> print_endline;
  Expectation.commit ();
  [%expect
    {|
    ((actual foo) (expected (foo)) (is_successful true)
     (character_range ((start_pos 162) (end_pos 182))))
    |}]
;;

let%expect_test "is_active" =
  Printf.printf "%b" (Expectation.is_active ());
  [%expect {| false |}];
  Printf.printf "foo";
  [%expectation {| foo |}];
  let was_active = Expectation.is_active () in
  Expectation.commit ();
  Printf.printf "%b" was_active;
  [%expect {| true |}];
  Printf.printf "%b" (Expectation.is_active ());
  [%expect {| false |}]
;;

let%expect_test "skipped" =
  let () = [%expectation.never_committed] in
  Expectation.skip ()
;;

let%expect_test "unreached" =
  let _ = fun () -> [%expectation.never_committed] in
  [%expect {| |}]
;;

let%expect_test "multiline and indentation matches" =
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
    Expectation.(is_successful (), expected (), actual ())
  in
  Expectation.commit ();
  print_endline (Bool.to_string is_successful);
  [%expect {| true |}];
  print_endline (Option.get expected);
  [%expect_exact "\n    line 1\n     line 2\n      line 3\n    \n"];
  print_endline actual;
  [%expect_exact "line 1\n line 2\n  line 3\n\n"]
;;

let%expect_test "output before committed" =
  print_endline "foo";
  [%expectation "foo"];
  print_endline "bar";
  let bar = [%expect.output] in
  Expectation.commit ();
  print_endline bar;
  [%expect {| bar |}]
;;
