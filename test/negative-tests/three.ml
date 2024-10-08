(*
   In old versions of [ppx_expect], all of the tests below would pass.

   Currently, [ppx_expect] instead enforces standardized formatting in [[%expect]] nodes,
   so we instead use this test to demonstrate that all of the below expectations are
   reformatted to the same thing.
*)

let%expect_test _ =
  let text_no_final_nl () = print_string "one\ntwo\nthree" in
  text_no_final_nl ();
  [%expect
    {|
  one
  two
  three|}];
  let text () = print_string "one\ntwo\nthree\n" in
  (* Base example *)
  text ();
  [%expect
    {|
  one
  two
  three
|}];
  (* ok to omit space between "expect" and "{" *)
  text ();
  [%expect
    {|
  one
  two
  three
|}];
  (* indentation allowed *)
  text ();
  [%expect
    {|
  one
  two
  three
|}]
;;
