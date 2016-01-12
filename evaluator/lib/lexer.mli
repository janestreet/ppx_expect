open Expectation


val parse_pretty : string -> Pretty.t
val parse_pretty_line : string -> Pretty.Line.t
val parse_body : string -> kind:Body.kind -> Body.t

val extract_quoted_string_terminators : string -> string list

(** Split on ['\n']. Includes all leading and trailing empty lines *)
val split_lines : string -> string list
