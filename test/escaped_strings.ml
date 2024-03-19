let%expect_test "escaped carriage return" =
  print_string "a\r\nb";
  [%expect " \n a\n b\n "];
  print_string "a\r\nb";
  [%expect_exact "a\r\nb"]
;;

let%expect_test "escaped tab" =
  print_string "a\tb";
  [%expect "a\tb"];
  print_string "a\tb";
  [%expect_exact "a\tb"]
;;

let%expect_test "escaped quote" =
  print_string "a\"b";
  [%expect "a\"b"];
  print_string "a\"b";
  [%expect_exact "a\"b"]
;;

let%expect_test "escaped trailing carriage return" =
  print_string "a\r\nb\r\n";
  [%expect " \n a\n b\n "];
  print_string "a\r\nb\r\n";
  [%expect_exact "a\r\nb\r\n"]
;;
