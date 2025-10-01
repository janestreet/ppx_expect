open! Base
open Ppx_expect_runtime_types [@@alert "-ppx_expect_runtime_types"]

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
      - If the test failed, set the current [Test_block]'s [fail] ref to [true] (so that
        the test harness will be informed of a failure at the conclusion of this
        [let%expect_test] block) *)
  val run_test : test_id:Expectation_id.t -> unit

  (** Given a test id:

      - Look up the [Test_node.t] with that id
      - Perform a consuming read of current test output, performing sanitization and
        checking for backtraces
      - Compare the consumed output with the output expected by the associated
        [Test_node.t]

      Unlike [run_test], [run_test_without_commiting] does not automatically record the
      test outcome or set the [fail] ref. Instead, user code must call exactly one of
      [For_external.Expectation.commit] or [For_external.Expectation.skip] below to commit
      or ignore the test result. *)
  val run_test_without_commiting : test_id:Expectation_id.t -> unit

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
        inline testing harness that the test has "failed". *)
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
    -> expected_exn:(Payload.t * Compact_loc.t) option
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
         (** An assoc list from unique IDs to [Test_node.t]s. These tests are registered
             in a global map for reachability checks. [Test_node.t]s are subsequently
             handled by their ID. *)
    -> (unit -> unit C.IO.t)
       (** A callback representing the RHS of the [let%expect_test] binding. *)
    -> unit
end

module For_external : sig @@ portable
  (** Functions for other libraries to interact with expect tests. *)

  (** If there is an expect test running, perform a consuming read of the current output
      and return it without any sanitization or backtrace-checking. Note that this is
      different from the behavior of [[%expect.output]], which does perform sanitization.

      If there is no test running, raise an error that includes [here]. *)
  val read_current_test_output_exn : here:Source_code_position.t -> string @@ nonportable

  val am_running_expect_test : unit -> bool

  (** The name of the currently-running expect-test.

      If there is no test running, raise an error that includes [here]. *)
  val current_expect_test_name_exn : here:Source_code_position.t -> string option

  val default_cr_for_multiple_outputs
    :  output_name:string
    -> outputs:string list
    -> string

  (** If the current test has reached a [[%expect]], [[%expect_exact]], or
      [[%expect.if_reached]] node whose output does not match the expected output, or if
      it has reached a [[%expect.unreachable]] node at all, returns [true].

      If the current test does not meet those criteria, returns [false].

      If there is no current test running, raises an error including [here]. *)
  val current_test_has_output_that_does_not_match_exn
    :  here:Source_code_position.t
    -> bool

  module Stack_frame : sig
    type t
  end

  module Match_or_mismatch : sig
    type t =
      | Match
      | Mismatch
  end

  (** Push current output onto a notional stack. Until the output is popped, it will be
      excluded from [read_current_test_output_exn]. *)
  val push_output_exn : here:Source_code_position.t -> Stack_frame.t

  (** Pop the top output stack frame, which must be the given frame. Returns [Mismatch] if
      the stack frame is not popped in the correct order. *)
  val pop_output_exn : here:Source_code_position.t -> Stack_frame.t -> Match_or_mismatch.t

  module Expectation : sig
    (** The functions below are more thoroughly documented in the public interface in
        [Expect_test_helpers_base] *)

    (** There is an active expectation from the time that test execution encounters an
        [[%expectation]] until one of [commit] or [skip] is called. *)
    val is_active : here:[%call_pos] -> unit -> bool

    (** Can be run exactly once upon encountering an [[%expectation]]. Accepts the test
        result as though that expectation was an [[%expect]]. *)
    val commit : here:[%call_pos] -> unit -> unit

    (** Can be run exactly once upon encountering an [[%expectation]]. Ignores the result
        for this [[%expectation]]. *)
    val skip : here:[%call_pos] -> unit -> unit

    (** Each of the following functions can be run arbitrarily many times between
        encountering an [[%expectation]] and calling [commit] or [skip] *)

    (** Produce a sexp representing the state of the most recently encountered
        [[%expectation]] *)
    val sexp_for_debugging : here:[%call_pos] -> unit -> Sexp.t

    (** Whether calling [commit] for the current [[%expectation]] would produce a test
        failure *)
    val is_successful : here:[%call_pos] -> unit -> bool

    (** Output collected at the current [[%expectation]] *)
    val actual : here:[%call_pos] -> unit -> string

    (** Output expected at the current [[%expectation]] *)
    val expected : here:[%call_pos] -> unit -> string option
  end
end

(** Action to perform when exiting from a program that runs expect tests. Alerts of
    runtime failure if the program exited while executing an expect test. *)
val at_exit : unit -> unit
