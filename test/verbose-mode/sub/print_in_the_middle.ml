let%expect_test "hello from expect" =
  print_endline "Hello from an expect test";
  [%expect {| Hello from an expect test |}]
;;

let () = print_endline "HELLO FROM REAL OCAML!"

let%expect_test "goodbye from expect" =
  print_endline "Goodbye from an expect test";
  [%expect {| Goodbye from an expect test |}]
;;
