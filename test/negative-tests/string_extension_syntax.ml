let%expect_test "correction for expect node" =
  print_endline "Testing";
  print_endline "1";
  print_endline "2";
  print_endline "3";
  {%expect xxx|what|xxx}
;;

let%expect_test "correction for expect exact node" =
  print_endline "Testing";
  print_endline "1";
  print_endline "2";
  print_endline "3";
  {%expect_exact|what|}
;;
