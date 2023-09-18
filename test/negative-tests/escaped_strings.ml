let%expect_test "escaped carriage return" =
  print_string "a\rb";
  [%expect ""];
  print_string "a\rb";
  [%expect_exact ""];
  print_string "a\r\nb";
  [%expect ""];
  print_string "a\r\nb";
  [%expect_exact ""];
  print_string "a\n\rb";
  [%expect ""];
  print_string "a\n\rb";
  [%expect_exact ""]
;;

let%expect_test "escaped tab" =
  print_string "a\tb";
  [%expect ""];
  print_string "a\tb";
  [%expect_exact ""];
  print_string "a\t\nb";
  [%expect ""];
  print_string "a\t\nb";
  [%expect_exact ""];
  print_string "a\n\tb";
  [%expect ""];
  print_string "a\n\tb";
  [%expect_exact ""]
;;

let%expect_test "escaped quote" =
  print_string "a\"b";
  [%expect ""];
  print_string "a\"b";
  [%expect_exact ""]
;;

let%expect_test "escaped trailing carriage return" =
  print_string "a\r\nb\r\n";
  [%expect ""];
  print_string "a\r\nb\r\n";
  [%expect_exact ""];
  print_string "a\r\nb\r\n";
  [%expect];
  print_string "a\r\nb\r\n";
  [%expect_exact]
;;

let%expect_test "unescaped carriage return --- empty expect" =
  print_string "a\r\nb";
  [%expect];
  print_string "a\r\nb";
  [%expect_exact]
;;

let%expect_test "unescaped carriage return --- populated expect" =
  print_string "a\r\nb";
  [%expect {|
    a
    b |}];
  print_string "a\r\nb";
  [%expect_exact {|a
b|}]
;;
