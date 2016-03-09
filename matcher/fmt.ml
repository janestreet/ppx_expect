open Sexplib.Std

type t =
  | Regexp  of string
  | Glob    of string
  | Literal of string
[@@deriving sexp_of, variants, compare]
