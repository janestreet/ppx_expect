We check that the [inline_tests_runner] explodes, even when not running tests.
This gives us reasonable confidence that, if we link this lib from another
executable, that executable will also explode if the tests are not elided
correctly.

  $ cd $TEST_DIR
  $ OCAMLRUNPARAM=b=0 ./inline_tests_runner -list-partitions
  Fatal error: exception Failure("entered test functor")
  [2]
