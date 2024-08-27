module Expect_test_config = struct
  include Expect_test_config

  let upon_unreleasable_issue = `Warning_for_collector_testing
end

let x = ref 0

[%%duplicate
  let%expect_test "what is x" =
    x := !x + 1;
    print_int !x;
    [%expect {| |}]
  ;;]
