external stdout_no_flush : unit -> unit = "stdout_from_stubs_but_dont_flush"
external stderr_no_flush : unit -> unit = "stderr_from_stubs_but_dont_flush"

let%expect_test _ =
  stdout_no_flush ();
  [%expect {| hello from c |}];
  stderr_no_flush ();
  [%expect {| error from c |}]
;;
