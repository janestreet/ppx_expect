open Core.Std
let%expect_test _ =
  (* Fails. Corrected .ml should contain [%expected_exact] *)
  Printf.printf " after space";
  [%expect {| after space|}]
