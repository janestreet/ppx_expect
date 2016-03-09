open Expect_test_common.Std

val match_expectation : Parsetree.expression -> Expectation.Raw.t option

val match_expect_test
  :  Parsetree.structure_item
  -> (bytes option * Parsetree.expression) option
