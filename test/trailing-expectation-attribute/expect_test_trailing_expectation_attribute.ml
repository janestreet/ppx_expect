let () = Printexc.record_backtrace false

let%expect_test "trailing attribute when disabled" = print_string "hello"
[@@expect.trailing {| hello |}]
;;

open struct
  module Expect_test_config = struct
    include Expect_test_config

    let trailing_output_syntax = `Attribute
  end
end

let%expect_test "explicit trailing attribute" = print_string "hello"
[@@expect.trailing {| hello |}]
;;

let%expect_test "trailing output with unreachable [%expect]" =
  print_endline "out1";
  if false then [%expect.unreachable]
[@@expect.trailing {| out1 |}]
;;

(* [[@@expect.trailing]] only captures output from a test that _finished_. When the test
   raises before finishing, the trailing output is folded into the [uncaught_exn]
   expectation (as it always has been) and an explicit [[@@expect.trailing]] attribute is
   deleted as unreachable; see the [negative-test] subdirectory for that correction. *)
let%expect_test "trailing attribute with uncaught exn" =
  print_endline "before exception";
  raise_notrace Exit
[@@expect.uncaught_exn
  {|
  Exit
  Trailing output
  ---------------
  before exception
  |}]
;;

open struct
  module Expect_test_config = struct
    include Expect_test_config

    let trailing_output_syntax = `Extension
  end
end

let%expect_test "explicit trailing attribute" = print_string "hello"
[@@expect.trailing {| hello |}]
;;

let%expect_test "explicit trailing extension" =
  print_string "hello";
  [%expect {| hello |}]
;;
