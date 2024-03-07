open! Base
open Types

(** Functor for building the runtime representation of a [let%expect_test] block *)

module Make (C : Expect_test_config_types.S) : sig
  (** Read test output, passing it through the configured sanitization function but not
      checking for backtraces. Equivalent to [[%expect.output]].

      Consuming read. *)
  val read_test_output_no_backtrace_check : unit -> string

  (** Given a test id:

      - Look up the [Test_node.t] with that id
      - Perform a consuming read of current test output, performing sanitization and
        checking for backtraces
      - Compare the consumed output with the output expected by the associated
        [Test_node.t]
      - Record the test outcome in the associated [Test_node.t]
      - If the test failed, set the current [Test_block]'s [fail] ref to [true] (so
        that the test harness will be informed of a failure at the conclusion of this
        [let%expect_test] block) *)
  val run_test : test_id:Expectation_id.t -> unit

  (** Execute a single [let%expect_test] block through [Ppx_inline_test_lib.test].

      - Assert that the test is defined in the currently-executing file.
      - Generate two implicit [Test_node.t]s representing trailing output (expectation
        that there is none) and uncaught exception (expectation that there is none if
        [expected_exn = None], or else that the test raises with [expected_exn]).
      - Add the explicit [Test_node.t]s in [expectations] (those actually present in the
        test body), as well as the two implicit [Test_node.t]s described above, into the
        global table of [Test_node.t]s.
      - Run the callback and the tests for trailing output and uncaught exceptions,
        accumulating the results of each reached "expectation" inside the corresponding
        [Test_node.t].
      - After the callback finishes, if any of the expectations do not match, inform the
        inline testing harness that the test has "failed".
  *)
  val run_suite
    :  filename_rel_to_project_root:string (** File in which the test is defined *)
    -> line_number:int (** Line number of the start of the test *)
    -> location:Compact_loc.t
         (** Range of characters of the entire test; printed if program exits unexpectedly *)
    -> trailing_loc:Compact_loc.t
         (** Where to insert the [[%expect]] for trailing output *)
    -> body_loc:Compact_loc.t
         (** Range of characters of the RHS of the [let%expect_test] binding. *)
    -> formatting_flexibility:Expect_node_formatting.Flexibility.t
         (** The formatting flexibility to use for the uncaught exn test. *)
    -> expected_exn:(Output.Payload.t * Compact_loc.t) option
         (** Contents of the [[@@expect.uncaught_exn]] node, if any. *)
    -> trailing_test_id:Expectation_id.t
         (** ID to use for the test checking that there is no trailing output. *)
    -> exn_test_id:Expectation_id.t
         (** ID to use for the test checking for uncaught exns. *)
    -> description:string option
         (** The string on the LHS of the [let%expect_test] binding, if any; passed to
        [Ppx_inline_test_lib]. *)
    -> tags:string list
         (** Test tags from the LHS of the [let%expect_test] binding; passed to
        [Ppx_inline_test_lib]. *)
    -> inline_test_config:Ppx_inline_test_lib.config
    -> expectations:(Expectation_id.t, Test_node.t) List.Assoc.t
         (** An assoc list from unique IDs to [Test_node.t]s. These tests are registered in a
        global map for reachability checks. [Test_node.t]s are subsequently handled by
        their ID. *)
    -> (unit -> unit C.IO.t)
       (** A callback representing the RHS of the [let%expect_test] binding. *)
    -> unit
end

module For_external : sig
  (** Functions for other libraries to interact with expect tests. *)

  (** If there is an expect test running, perform a consuming read of the current output
      and return it without any sanitization or backtrace-checking. Note that this is
      different from the behavior of [[%expect.output]], which does perform sanitization.

      If there is no test running, raise an error that includes [here].
  *)
  val read_current_test_output_exn : here:Source_code_position.t -> string

  val am_running_expect_test : unit -> bool

  val default_cr_for_multiple_outputs
    :  output_name:string
    -> outputs:string list
    -> string
end

(** Action to perform when exiting from a program that runs expect tests. Alerts of
    runtime failure if the program exited while executing an expect test. *)
val at_exit : unit -> unit
