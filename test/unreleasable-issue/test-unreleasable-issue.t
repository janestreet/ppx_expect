Check all reporting modes without checking real CRs into the test output.
With empty expectations, the tests must fail in every mode.

  $ cd $TEST_DIR/sub
  $ for mode in default CR_for_child Warning_for_collector_testing; do
  >   echo "$mode"
  >   rm -f unreleasable_issue.ml.corrected
  >   PPX_EXPECT_TEST_UNRELEASABLE_ISSUE="$mode" ./inline_tests_runner -no-color > output 2>&1
  >   echo "exit status: $?"
  >   sed -n '/(\* .*expect_test.*:/p' unreleasable_issue.ml.corrected | sed 's/CR /XX /g'
  >   rm -f unreleasable_issue.ml.corrected
  > done
  default
  exit status: 2
      (* XX expect_test_collector: This test expectation appears to contain a backtrace.
  CR_for_child
  exit status: 2
      (* XX expect_test_collector for $child: This test expectation appears to contain a backtrace.
  Warning_for_collector_testing
  exit status: 2
        (* expect_test: Test ran multiple times with different test outputs *)
      (* expect_test_collector: This test expectation appears to contain a backtrace.
