(** Determine whether a test's output matches its expected output. *)

type 'a t =
  | Match
  | Correction of 'a

val expectation :
  expect         : Expectation.t ->
  actual         : string        ->
  default_indent : int           ->
  Expectation.t t

