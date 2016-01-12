open! Core.Std

let%expect_test _ =
  [%expect {| hi ho |}];
  failwith "hi ho";
  [%expect {| it's off to work we go |}]
;;
