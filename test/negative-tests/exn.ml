open! Core

let%expect_test _ =
  [%expect {| hi ho |}];
  Printexc.record_backtrace false;
  ignore (failwith "hi ho" : unit);
  [%expect {| it's off to work we go |}]
;;

let%expect_test _ =
  Printexc.record_backtrace false;
  ignore (failwith "hi ho" : unit);
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
  (Failure "hi ho")
|}]
;;
