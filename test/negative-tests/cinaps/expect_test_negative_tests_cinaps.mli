open! Base

val generate
  :  postprocess_test_output:string option
  -> set_up_for_tests:string option
  -> inline_test_args:string option
  -> exclude_targets:string list
  -> unit
