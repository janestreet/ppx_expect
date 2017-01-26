open! Core

let%expect_test _ =
  [%expect {| hi ho |}];
  Printexc.record_backtrace false;
  failwith "hi ho";
  [%expect {| it's off to work we go |}]
;;
