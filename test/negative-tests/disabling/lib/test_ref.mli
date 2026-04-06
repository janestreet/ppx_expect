type t =
  | Init
  | Set_by_inline_test
[@@deriving sexp, compare ~localize]

val value : unit -> t
