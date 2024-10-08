(*
   In old versions of [ppx_expect], all of the tests below would pass.

   Currently, [ppx_expect] instead enforces standardized formatting in [[%expect]] nodes,
   so we instead use this test to demonstrate that all of the below expectations are
   reformatted to the same thing.
*)

let%expect_test _ =
  let module _ = struct
    let () =
      print_string "hello";
      [%expect {| hello |}]
    ;;

    let () =
      print_string "hello\n";
      [%expect
        {|
                                              hello
                                            |}]
    ;;

    let () =
      print_string "hello\n\n";
      [%expect
        {|
                                              hello

                                            |}]
    ;;

    let () =
      print_string "\nhello";
      [%expect
        {|

                                              hello|}]
    ;;

    let () =
      print_string "\nhello\n";
      [%expect
        {|

                                              hello
                                            |}]
    ;;

    let () =
      print_string "\nhello\n\n";
      [%expect
        {|

                                              hello

                                            |}]
    ;;

    let () =
      print_string "\n\nhello";
      [%expect
        {|


                                              hello|}]
    ;;

    let () =
      print_string "\n\nhello\n";
      [%expect
        {|


                                              hello
                                            |}]
    ;;

    let () =
      print_string "\n\nhello\n\n";
      [%expect
        {|


                                              hello

                                            |}]
    ;;
  end
  in
  ()
;;
