let run_test () =
  let module _ = struct
    let%expect_test "" = assert false
  end
  in
  ()
;;

let%expect_test ("" [@tags "fast-flambda"]) = run_test ()

(* This test is still silently ignored when running with [-require-tag fast-flambda], but
   I don't think there's much we can do about that. It will still complain about nesting
   when normal testing runs. *)
let run_test () =
  let module _ = struct
    let%expect_test ("" [@tags "fast-flambda"]) = assert false
  end
  in
  ()
;;

let%expect_test "" = run_test ()
