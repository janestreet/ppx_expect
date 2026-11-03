let () = Printexc.record_backtrace false

open struct
  module Expect_test_config = struct
    include Expect_test_config

    let trailing_output_syntax = `Attribute
  end
end

let%expect_test "implicit trailing output uses attribute correction" =
  print_string "hello"
;;

let%expect_test "explicit trailing attribute is checked" = print_string "goodbye"
[@@expect.trailing {| wrong |}]
;;

let%expect_test "trailing attribute with uncaught exn" =
  print_endline "before exception";
  raise_notrace Exit
[@@expect.trailing {| wrong trailing |}] [@@expect.uncaught_exn {| wrong exn |}]
;;

let%expect_test "implicit, with uncaught exn" =
  print_endline "before exception";
  raise_notrace Exit
;;

open struct
  module Expect_test_config = struct
    include Expect_test_config

    let trailing_output_syntax = `Extension
  end
end

let%expect_test "implicit trailing output uses attribute correction" =
  print_string "hello"
;;

let%expect_test "explicit trailing attribute is checked" = print_string "goodbye"
[@@expect.trailing {| wrong |}]
;;
