open! Base
open Types

module Definitions = struct
  module Insert_loc = struct
    (** Whether this expectation is tied to an AST node already present in the source, and
        the location information needed to determine where to insert corrections for this
        expectation *)
    type t =
      | Overwrite of
          { whole_node : Compact_loc.t
          ; payload : Compact_loc.t option
          }
          (** An expectation parsed from the test file and which should be overwritten by
          corrections. Corrections to just the payload should overwrite just the [payload]
          location, if present. If no [payload] location is present, or for corrections
          that change the entire node (e.g. a change from [[%expect _]] to
          [[%expect.unreachable]]), overwrite the [whole_node] loc. *)
      | Insert of Virtual_loc.t
          (** An expectation not parsed from the file that should be inserted into
          [Virtual_loc.loc] and is associated with a test whose body is at
          [Virtual_loc.body_loc] *)
  end

  module Behavior_type = struct
    (** The type of expect node *)
    type t =
      [ `Expect
      | `Unreachable
      ]
  end

  module Expect_reachability = struct
    (** Whether an expect node expresses an assertion that control flow passes through it
        every time a test is run *)
    type t =
      | Can_reach
          (** Test passes even if node is only reached on *some* executions of a test *)
      | Must_reach (** Test fails unless node is reached by *all* executions of a test *)
  end

  module On_unreachable = struct
    (** What should be done if this expectation is never reached in the test execution *)
    type t =
      | Silent (** Do nothing *)
      | Delete (** Delete this expectation from the source file *)
      | Replace_with_unreachable
          (** Replace this expectation with a [[%expect.unreachable]] node *)
  end

  module Behavior = struct
    (** A ['behavior_type t] describes how to handle a test node when running tests
        and writing corrections.

        ['behavior_type] determines the types of rewrites that are possible at this node.
        It is either [`Expect] (indicating that both corrections for unexpected output and
        rewrites for unreachability are possible) or [`Unreachable] (indicating that only
        corrections for unexpected output are possible).
    *)
    type _ t =
      | Expect :
          { payload : Output.Payload.t
          ; on_unreachable : On_unreachable.t
          ; reachability : Expect_reachability.t
          }
          -> [ `Expect ] t
      | Unreachable :
          { reachability_of_corrected : Expect_reachability.t
              (** The reachability of the node inserted if this unreachable node is
              unexpectedly reached *)
          }
          -> [ `Unreachable ] t
  end

  (** A [('behavior_type) t] carries information about how to run tests for a
      specific expect node and rewrite it in the source file if there are corrections. The
      ['behavior_type] type variable has the same meanings as in
      ['behavior_type Behavior.t].
  *)
  type 'behavior_type t =
    { position : Insert_loc.t
    ; behavior : 'behavior_type Behavior.t
    ; payload_type : Output.Type.t
    ; on_incorrect_output : String_node_format.Shape.t
        (** The name and syntax style of the extension point or attribute used to write
        corrections when receiving "incorrect" output for this test node. For each [t],
        there is only one such node. For example, if an [{%expect_exact||}] node is
        reached with incorrect output, it is always corrected to a different
        [{%expect_exact||}] node, and an [[%expect.unreachable]] that is reached is
        always corrected to an [[%expect]] node.

        Note that for a node that should be reachable, the correction when it is found to
        be unreachable is instead governed by [on_unreachable] in the [Expect] constructor
        of [behavior].
    *)
    ; inconsistent_outputs_message : string
    }
end

module type Expectation = sig
  include module type of struct
    include Definitions
  end

  module Insert_loc : sig
    include module type of struct
      include Insert_loc
    end

    val loc : Insert_loc.t -> Compact_loc.t
  end

  val with_behavior : 'old_behavior t -> 'new_behavior Behavior.t -> 'new_behavior t

  (** [formatter ~expect_node_formatting t] returns the [Output.Formatter.t] that formats
      test output according to the type ([exact] or [pretty]) of [t], using information
      about the location and payload of [t] for formatting. *)
  val formatter
    :  expect_node_formatting:Expect_node_formatting.t
    -> _ t
    -> Output.Formatter.t

  (** [[%expect _]] *)
  val expect
    :  formatting_flexibility:Expect_node_formatting.Flexibility.t
    -> node_loc:Compact_loc.t
    -> located_payload:(Output.Payload.t * Compact_loc.t) option
    -> [ `Expect ] t

  (** [[%expect_exact _]] *)
  val expect_exact
    :  formatting_flexibility:Expect_node_formatting.Flexibility.t
    -> node_loc:Compact_loc.t
    -> located_payload:(Output.Payload.t * Compact_loc.t) option
    -> [ `Expect ] t

  (** [[%expect.unreachable]] *)
  val expect_unreachable : node_loc:Compact_loc.t -> [ `Unreachable ] t

  (** [[@@expect.uncaught_exn _]] *)
  val expect_uncaught_exn
    :  formatting_flexibility:Expect_node_formatting.Flexibility.t
    -> node_loc:Compact_loc.t
    -> located_payload:(Output.Payload.t * Compact_loc.t) option
    -> [ `Expect ] t

  (** Runtime representation of the implicit [[%expect {||}]] at the end of every expect
      test. *)
  val expect_trailing : insert_loc:Virtual_loc.t -> [ `Expect ] t

  (** Runtime representation of the assertion that a test does not produce uncaught
      exceptions, which a user implicitly makes by omitting an [[@@expect.uncaught_exn _]]
      attribute. *)
  val expect_no_uncaught_exn : insert_loc:Virtual_loc.t -> [ `Unreachable ] t

  module For_apply_style : sig
    type format_payload :=
      expect_node_formatting:Expect_node_formatting.t
      -> payload_loc:Compact_loc.t
      -> node_loc:Compact_loc.t
      -> String_node_format.Delimiter.t
      -> string
      -> string option

    val format_expect_payload : format_payload
    val format_uncaught_exn_payload : format_payload
  end
end
