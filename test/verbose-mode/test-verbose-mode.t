The output is being tee'd in "real time"

  $ cd $TEST_DIR
  $ ./sub/inline_tests_runner -verbose -only-test print_in_the_middle.ml 2>&1 | sed -E 's/[0-9]+/X/g'
  File "print_in_the_middle.ml", line X, characters X-X: hello from expect
  Hello from an expect test
  
   (X.X sec)
  HELLO FROM REAL OCAML!
  File "print_in_the_middle.ml", line X, characters X-X: goodbye from expect
  Goodbye from an expect test
  
   (X.X sec)

The output is tee'd even if the test gets "stuck"

  $ timeout 1 ./sub/inline_tests_runner -verbose -only-test test_loops.ml 2>&1 | sed -E 's/[0-9]+/X/g'
  HELLO FROM REAL OCAML!
  File "test_loops.ml", line X, characters X-X: doesn't finish
  about to enter an infinite loop

These parts of the output go to STDERR

  $ ./sub/inline_tests_runner -verbose -only-test print_in_the_middle.ml > /dev/null

These parts of the output go to STDOUT

  $ ./sub/inline_tests_runner -verbose -only-test print_in_the_middle.ml 2> /dev/null | sed -E 's/[0-9]+/X/g' >&2
  File "print_in_the_middle.ml", line X, characters X-X: hello from expect
  Hello from an expect test
  
   (X.X sec)
  HELLO FROM REAL OCAML!
  File "print_in_the_middle.ml", line X, characters X-X: goodbye from expect
  Goodbye from an expect test
  
   (X.X sec)

These parts of the output go to STDERR (using -verbose-to-stderr flag)

  $ ./sub/inline_tests_runner -verbose-to-stderr -only-test print_in_the_middle.ml > /dev/null 2> >(sed -E 's/[0-9]+/X/g' >&2)
  File "print_in_the_middle.ml", line X, characters X-X: hello from expect (X.X sec)
  File "print_in_the_middle.ml", line X, characters X-X: goodbye from expect (X.X sec)

These parts of the output go to STDOUT (using -verbose-to-stderr flag)

  $ ./sub/inline_tests_runner -verbose-to-stderr -only-test print_in_the_middle.ml 2> /dev/null
  
  Hello from an expect test
  
  HELLO FROM REAL OCAML!
  
  Goodbye from an expect test
  
