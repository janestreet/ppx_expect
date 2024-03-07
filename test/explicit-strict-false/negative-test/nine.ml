(* Show that, even when compiling with [-expect-test-strict-indentation=false], the issued
   corrected files makes the "fixed" expect blocks satisfy standard indentation rules *)

let%expect_test _ =
  let module _ = struct
    let () =
      print_string "hello";
      [%expect
        {|
                                              goodbye
                                            |}]
    ;;

    let () =
      print_string "hello\n";
      [%expect
        {|
                                              goodbye
                                            |}]
    ;;

    let () =
      print_string "hello\n\n";
      [%expect
        {|
                                              goodbye

                                            |}]
    ;;

    let () =
      print_string "\nhello";
      [%expect {|

                                              goodbye|}]
    ;;

    let () =
      print_string "\nhello\n";
      [%expect
        {|

                                              goodbye
                                            |}]
    ;;

    let () =
      print_string "\nhello\n\n";
      [%expect
        {|

                                              goodbye

                                            |}]
    ;;

    let () =
      print_string "\n\nhello";
      [%expect {|


                                              goodbye|}]
    ;;

    let () =
      print_string "\n\nhello\n";
      [%expect
        {|


                                              goodbye
                                            |}]
    ;;

    let () =
      print_string "\n\nhello\n\n";
      [%expect
        {|


                                              goodbye

                                            |}]
    ;;
  end
  in
  ()
;;
