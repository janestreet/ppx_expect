## 113.43.00

- Always flush Pervasives.stdout in the ppx_expect runtime.

  We already do this, but it was missing in one place.

- Made the test framework resilient to user changing the current working directory during the test.

- Print newlines in `"`-strings as real newlines, not `\n`

- The expect test runtime breaks any executable that wants to work even if
  cwd doesn't exist, like fe does. Fix that.

  It also brings expect tests in line with what ppx\_inline\_test does, and removes the diff
  due to absolute paths I was seeing in the output of `./inline_tests_runner -log` in some
  other features. Concretely, here is what changes:

- Use the new context-free API

- Change the check in ppx\_expect to be a dynamic check. Instead of
  checking that expect tests appears only at toplevel, we test that
  they are run in the library they appear.

  This has several consquence:

  - ppx\_expect can use `Context_free` as well and doesn't require two extra passes
  - expect tests can appear inside let%test_module

## 113.33.01

- Add dependency on `re.emacs`

## 113.33.00

- Don't remove trailing semicolons when producing a correction.

- Corrected `%expect`s with double quoted strings don't have the single space padding.

- In the ppx\_expect runtime, flush stdout before redirecting it
  This is to avoid capturing leftover of the stdout buffer.

- Make sure the expect-test runtime doesn't generate
  `%collector_never_triggered`, which is not accepted by ppx\_expect.
  Instead generate:

    `%expect {| DID NOT REACH THIS PROGRAM POINT |}`

- Make expect tests pass the user description to the inline test runtime

- Fix a race condition in the ppx\_expect runtime


- Change ppx\_expect be more permissive when matching whitespace in actual output.
  See `ppx/ppx_expect/README.org` for details.

  Changes to the implementation of ppx\_expect (including some refactoring):
  - factorized the common bits between the runtime and ppx rewriter
    into one library expect_test_common
  - factorized different structures representing the same thing using polymorphism
  - communicate data structures between the ppx rewriter and runtime
    using a generated lifter instead of hand-written lifters
  - splitted the matching and correction writing code: the .corrected is
    now only created when needed instead of all the time
  - added a concrete syntax tree to represent both the actual output and
    expectation in non-exact mode.
    This allow to keep the user formatting as much as possible
  - made various bits more re-usable

- Change the default style of multi-line expectation to:

    `%expect {|
      abc
      def |}`

  More generally, try to preserve the formatting a bit more when
  correcting from empty or single to multi-line.

- Arrange things so that when `open Async.Std` is opened, `%expect ...`
  expressions are of type `unit Deferred.t` and flush stdout before
  capturing the output.

## 113.24.00

Initial release.
