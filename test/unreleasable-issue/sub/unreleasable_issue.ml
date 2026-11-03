module Expect_test_config = struct
  include Expect_test_config

  let upon_unreleasable_issue =
    match Sys.getenv "PPX_EXPECT_TEST_UNRELEASABLE_ISSUE" with
    | "default" -> upon_unreleasable_issue
    | "CR_for_child" -> `CR_for_child
    | "Warning_for_collector_testing" -> `Warning_for_collector_testing
    | _ -> assert false
  ;;
end

let%expect_test "multiple outputs" =
  let test output =
    print_endline output;
    [%expect {| |}]
  in
  test "foo";
  test "bar"
;;

let%expect_test "backtrace" =
  print_endline "Raised at example";
  [%expect {| |}]
;;
