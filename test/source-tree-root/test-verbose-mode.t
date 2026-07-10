Check that a test executable copied to a new location can still run against the
files in its original location when the [-source-tree-root] flag is passed.

Test relative path

  $ cd $TEST_DIR
  $ dir=foo/bar/verbose-mode-test-dir
  $ mkdir -p $dir
  $ cd $dir
  $ cp $TEST_DIR/inline_tests_runner* .
  $ ./inline_tests_runner -source-tree-root ../../.. -no-color 2>&1 | tail -n +4
   |let%expect_test _ =
   |  print_endline "hello world";
  -|  [%expect {| |}]
  +|  [%expect {| hello world |}]
   |;;

  $ cd $TEST_DIR
  $ ls *.corrected
  expect_test_source_tree_test.ml.corrected
  $ rm *.corrected
  $ cd $dir

Test absolute path

  $ ./inline_tests_runner -source-tree-root $TEST_DIR -no-color 2>&1 | tail -n +4
   |let%expect_test _ =
   |  print_endline "hello world";
  -|  [%expect {| |}]
  +|  [%expect {| hello world |}]
   |;;

  $ cd $TEST_DIR
  $ ls *.corrected
  expect_test_source_tree_test.ml.corrected
