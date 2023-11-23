let%expect_test "short quoted string" =
  print_string "foo\nbar\n";
  [%expect ""]
;;

let%expect_test "long quoted string" =
  print_string
    {|
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXX
    |};
  [%expect ""]
;;

let%expect_test "quoted strings with leading spaces" =
  print_string {|
    live
      long
        and
    prosper
    |};
  [%expect ""];
  print_string
    {|
    live
      loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
        and
    prosper
    |};
  [%expect ""]
;;
