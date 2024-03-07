open! Base
open Types

module Type : sig
  (** How the output expected at a node interacts with whitespace. *)
  type t =
    | Exact (** Matches the captured output exactly, including whitespace. *)
    | Pretty
        (** Matches a version of the captured output that has been formatted according to
        standard rules about indentation and other whitespace. *)
end

(** Captured output that has been formatted according to the rules of the [Type.t] of
    its corresponding node. *)
module Formatted : T

(** Captured test output, possibly from multiple tests, after it has been formatted and
    compared to the original contents of the node; the contents that will be written to
    the node in the corrected file. *)
module Reconciled : T

module Formatter : sig
  (** A [Formatter.t] describes how to convert captured [string] output into a
      [Formatted.t]. *)
  type t

  (** Given a function that applies the desired format to a [string] representing captured
      output, create a [Formatter.t]. *)
  val create : (string -> string) -> t

  (** Apply a [Formatter.t] to a [string] representing captured output to produce a
      [Formatted.t]. *)
  val apply : t -> string -> Formatted.t
end

module Test_result : sig
  (** The outcome produced by a single expect node when it is reached. [Pass] if the real
      output matches the expected output, otherwise [Fail s], where [s] is the real
      output. *)
  type t = private
    | Pass
    | Fail of Reconciled.t

  val compare : t -> t -> int
end

module Payload : sig
  (** Payloads given as arguments to expectation AST nodes. *)
  type t =
    { contents : string
        (** The contents of the payload; the output expected at some node, as a raw [string]
        exactly as they were parsed from the source file. *)
    ; tag : String_node_format.Delimiter.t (** The delimiters used in the payload. *)
    }

  (** Add the default tags to a payload. *)
  val default : string -> t

  (** The source-code representation of a payload. *)
  val to_source_code_string : t -> string
end

(** Returns [Pass] if [test_output] is considered to match [expected_output]; otherwise
    returns [Fail test_output]. *)
val reconcile : expected_output:string -> test_output:Formatted.t -> Test_result.t

(** Given some [Formatted.t] output that always indicates test failure (e.g., an
    inconsistent outputs message), produces a failing test result with the corresponding
    "reconciled" output. *)
val fail : Formatted.t -> Test_result.t

(** The new payload represented by a reconciled expectation.

    If [tag] is not compatible with the new payload contents (for example, the tag
    represents a [{x| delimited string |x}] and the new contents contain ["|x}"]), the tag
    is adjusted so the resulting payload can be parsed.
*)
val to_formatted_payload : tag:String_node_format.Delimiter.t -> Reconciled.t -> Payload.t

(** The source-code representation of a reconciled expect node.

    If [tag] is not compatible with the new payload contents (for example, the tag
    represents a [{x| delimited string |x}] and the new contents contain ["|x}"]), the tag
    is adjusted so the resulting payload can be parsed.
*)
val to_source_code_string
  :  expect_node_formatting:Expect_node_formatting.t
  -> node_shape:String_node_format.Shape.t
  -> tag:String_node_format.Delimiter.t
  -> Reconciled.t
  -> string
