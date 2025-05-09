module Expect_test_config = struct
  include Expect_test_config

  let upon_unreleasable_issue = `Warning_for_collector_testing
end

module Expectation = Ppx_expect_runtime.For_external.Expectation
[@@alert "-ppx_expect_runtime"]

let%expect_test _ =
  let f output =
    print_string output;
    [%expect {| hello world |}]
  in
  f "foo";
  f "bar"
;;

let%expect_test "same expectation with multiple inconsistent outputs" =
  let test output =
    print_endline output;
    [%expectation {||}];
    Expectation.commit ()
  in
  test "foo";
  test "bar"
;;
