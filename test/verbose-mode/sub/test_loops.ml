let rec loop () = loop ()

let%expect_test "doesn't finish" =
  print_endline "about to enter an infinite loop";
  loop ()
;;
