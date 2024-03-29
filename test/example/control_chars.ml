open Core

(* This test contains control chars literally in the ML source.  Keep this test separate
   from other tests in [tests.ml] because the control chars seem to provoke odd behaviour
   when running commands like [hg diff] *)

let%expect_test _ =
  let chars0_to_32 () =
    let s =
      List.range 0 32
      |> List.map ~f:(fun i -> String.of_char (Char.of_int_exn i))
      (* We use a [sep] to avoid having a trailing tab char in the expected output, which
         would be hard to write below, because our editors trim trailing whitespace. *)
      |> String.concat ~sep:"x"
    in
    print_string s
  in
  chars0_to_32 ();
  [%expect_exact
    "\000x\001x\002x\003x\004x\005x\006x\007x\008x\009x\010x\011x\012x\013x\014x\015x\016x\017x\018x\019x\020x\021x\022x\023x\024x\025x\026x\027x\028x\029x\030x\031"];
  chars0_to_32 ();
  [%expect_exact " xxxxxxxxx\tx\nxxxxxxxxxxxxxxxxxxxxx"];
  chars0_to_32 ();
  [%expect
    {|
     xxxxxxxxx	x
    xxxxxxxxxxxxxxxxxxxxx
    |}]
;;
