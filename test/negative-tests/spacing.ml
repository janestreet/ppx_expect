open Core

let%expect_test _ =
  let text_no_final_nl () = print_string "one\ntwo(no newline)\nthree" in
  text_no_final_nl ();
  [%expect
    {|

  one
  two(no newline)

  three
  |}];
  (* take an integer tag to [text] help the different tests be distinguished somewhat in the
     .expected.patdiff *)
  let text n = Printf.printf "one\ntwo(%d)\nthree\n" n in
  text 1;
  [%expect
    {|
  one
  two(1)
  three|}];
  text 2;
  [%expect
    {|
    one
  two(2)
    three
  |}];
  (* Check that it reindents expectation properly *)
  printf "  one\n blah\n  three";
  [%expect
    {|
      one
    two
      three
  |}]
;;
