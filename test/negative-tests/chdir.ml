let%expect_test _ =
  print_string "About to change dir";
  Sys.mkdir "tmp" 0o755;
  Sys.chdir "tmp";
  Sys.rmdir "../tmp";
  [%expect]
;;
