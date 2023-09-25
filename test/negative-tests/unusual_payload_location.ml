let%expect_test _ =
  print_endline "Does it get moved?";
  print_endline "Let's hope not.";
  [%expect {| Do not move this payload |}]
;;

let%expect_test _ =
  print_endline "Does it get moved?";
  print_endline "Let's hope not.";
  [%expect_exact {| Do not move this payload |}]
;;
