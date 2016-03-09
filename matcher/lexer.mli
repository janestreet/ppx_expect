open Expect_test_common.Std

(** Strip all surrounding whitespace and return the result as a list of lines *)
val strip_surrounding_whitespaces : string -> unit Cst.t


val parse_pretty      : string -> Fmt.t Cst.t
val parse_pretty_line : string -> Fmt.t
val parse_body        : string Expectation.Body.t -> Fmt.t Cst.t Expectation.Body.t

val extract_quoted_string_terminators : string -> string list
