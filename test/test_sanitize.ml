let%expect_test "no sanitization" =
  print_endline "hi!";
  [%expect {| hi! |}]
;;

module%test _ = struct
  module Expect_test_config = struct
    include Expect_test_config

    let sanitize s = if s = "" then "" else "local module sanitize: " ^ s
  end

  let%expect_test "local sanitize" =
    print_endline "hi!";
    [%expect {| local module sanitize: hi! |}]
  ;;
end

let%expect_test "again no sanitization" =
  print_endline "hi!";
  [%expect {| hi! |}]
;;

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = if s = "" then "" else "SANITIZED: " ^ s
end

let%expect_test "sanitize" =
  print_endline "hi!";
  [%expect {| SANITIZED: hi! |}]
;;

let%expect_test "we don't sanitize errors" =
  Printexc.record_backtrace false;
  (failwith "hi!" : unit);
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure hi!) |}]
;;
