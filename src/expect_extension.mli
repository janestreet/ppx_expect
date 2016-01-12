
type t =
  { expected : string
  ; tag      : string option
  ; is_exact : bool
  }

val match_expectation : Parsetree.expression -> t option

val match_expect_test
  :  Parsetree.structure_item
  -> (bytes option * Parsetree.expression) option
