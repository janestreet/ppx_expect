open Sexplib.Std
open Ppx_compare_lib.Builtin

type t =
  | Regexp  of string
  | Glob    of string
  | Literal of string
[@@deriving sexp_of, variants, compare]
