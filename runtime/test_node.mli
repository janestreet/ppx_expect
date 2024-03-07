open! Base
open Types

(** Accumulator of test results for one expect node *)
type t

module Create : sig
  (** Functions for creating [t]s corresponding to each of the test nodes that can be
      parsed out of extension points in a [let%expect_test].

      Each of these creators accepts the location of the entire AST node associated with
      the e.g. [[%expect]] test.
  *)

  (** [[%expect _]] *)
  val expect
    :  formatting_flexibility:Expect_node_formatting.Flexibility.t
         (** If tests should be flexible about formatting rules, the formatting rules that
        define this flexibility *)
    -> node_loc:Compact_loc.t (** Location of the [[%expect _]] node *)
    -> located_payload:(Output.Payload.t * Compact_loc.t) option
         (** The string payload and its location, if there is one *)
    -> t

  (** [[%expect_exact _]] *)
  val expect_exact
    :  formatting_flexibility:Expect_node_formatting.Flexibility.t
         (** If tests should be flexible about formatting rules, the formatting rules that
        define this flexibility *)
    -> node_loc:Compact_loc.t (** Location of the [[%expect_exact _]] node *)
    -> located_payload:(Output.Payload.t * Compact_loc.t) option
         (** The string payload and its location, if there is one *)
    -> t

  (** [[%expect.unreachable]] *)
  val expect_unreachable
    :  node_loc:Compact_loc.t (** Location of the [[%expect.unreachable]] node *)
    -> t
end

(** Functions exported for use in other modules of the expect test runtime. *)

val of_expectation : [< Expectation.Behavior_type.t ] Expectation.t -> t

(** Updates reachedness information for [t]. *)
val record_end_of_run : t -> unit

(** Records the result of receiving output [test_output_raw] at [t], using
    [expect_node_formatting] to format the correction if necessary. If the output results
    in a correction, sets [failure_ref := true]. We use a [bool ref] argument instead of a
    [bool] return value to decrease the chance that failures in tests are accidentally
    dropped and make it more likely that they are correctly reported to e.g. the inline
    test runner harness. *)
val record_result
  :  expect_node_formatting:Expect_node_formatting.t
  -> failure_ref:bool ref
  -> test_output_raw:string
  -> t
  -> unit

module Global_results_table : sig
  type node := t
  type postprocess := node list Write_corrected_file.Patch_with_file_contents.t

  (** Given an assoc list mapping [Expectation_id.t]s to fresh [Test_node.t]s, the
      [absolute_filename] of the file whence these tests originate, and the [postprocess]
      closure to run after all tests in that file finish:

      1. Store the [postprocess] closure for [absolute_filename]

      2. Add each test to the global tests registry if no test with that id has been
      registered for [absolute_filename]

      3. For each test id, reset [reached_this_run] for that test

      4. Return an assoc list from [Expectation_id.t]s to the [Test_node.t]s that will
      actually be used during testing; for each test, this is the same [Test_node.t] that
      was passed in if that test has not yet been registered, and otherwise the
      [Test_node.t] that was already in the table
  *)
  val initialize_and_register_tests
    :  absolute_filename:string
    -> (Expectation_id.t, node) List.Assoc.t
    -> postprocess
    -> (Expectation_id.t, node) List.Assoc.t

  val find_test : absolute_filename:string -> test_id:Expectation_id.t -> node

  val process_each_file
    :  f:(filename:string -> test_nodes:node list -> postprocess:postprocess -> 'a)
    -> 'a list
end

module For_mlt : sig
  (** Functions exported for use in toplevel expect tests *)

  (** The string that this test node "expects" if it is an [[%expect]] or
      [[%expect_exact]] node. [None] if it is an [[%expect.unreachable]]. *)
  val expectation_of_t : t -> string option

  (** Records the test result of receiving the raw test output [test_output_raw]. If the
      test "fails" (the output is not considered to match the expectation), sets
      [failure_ref := true] and returns the number of lines that will be spanned by
      inserted correction. If the test "passes", does not update [failure_ref] and returns
      [None]. *)
  val record_and_return_number_of_lines_in_correction
    :  expect_node_formatting:Expect_node_formatting.t
    -> failure_ref:bool ref
    -> test_output_raw:string
    -> t
    -> int option

  (** The location of the AST extension node associated with this test. *)
  val loc : t -> Compact_loc.t

  (** Retrieves the corrections that need to be made to the original source file based on
      the test results collected in this test node so far. Returns a list of pairs
      containing the location of the character range to overwrite and the string to write
      at that location. *)
  val to_diffs
    :  cr_for_multiple_outputs:(output_name:string -> outputs:string list -> string)
    -> expect_node_formatting:Expect_node_formatting.t
    -> original_file_contents:string
    -> t
    -> (Compact_loc.t * string) list
end
