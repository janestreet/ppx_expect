open Expect_test_common.Std

module Test_outcome : sig
  (** Outcome of a group of test. Either a single [let%expect_test], or a whole file for
      toplevel expect test. *)
  type t =
    { expectations    : Fmt.t Cst.t Expectation.t File.Location.Map.t
    ; saved_output    : string File.Location.Map.t
    ; trailing_output : string
    }
end

module Test_correction : sig
  (** Correction for one [Test_outcome.t] *)
  type t

  val map_corrections : t -> f:(Fmt.t Cst.t -> Fmt.t Cst.t) -> t

  (** Single node correction *)
  type node_correction =
    | Collector_never_triggered
    | Correction of Fmt.t Cst.t Expectation.Body.t

  val make
    :  location        : File.Location.t
    -> corrections     : (Fmt.t Cst.t Expectation.t * node_correction) list
    -> trailing_output : Fmt.t Cst.t Expectation.Body.t Reconcile.Result.t
    -> t Reconcile.Result.t
end

(** Evaluate the results of all the tests run through Expect_test_runner. *)
val evaluate_test
  :  file_contents:string
  -> location:File.Location.t
  -> Test_outcome.t
  -> Test_correction.t Reconcile.Result.t

type mode = Inline_expect_test | Toplevel_expect_test

(** Write a list of correction to a file. *)
val write_corrected
  :  file:string
  -> file_contents:string
  -> mode:mode
  -> Test_correction.t list
  -> unit

(** Diff two files, using [patdiff] with a fallback to [diff] if [patdiff] produces no
    differences. *)
val print_diff : file1:string -> file2:string -> use_color:bool -> unit
