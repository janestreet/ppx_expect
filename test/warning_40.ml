[@@@warning "+40"]

let%expect_test _ =
  print_endline "hello";
  [%expect {| hello |}];
  print_endline "world";
  [%expect_exact
    {|world
|}];
  let _f () = [%expect.unreachable] in
  ()
;;
