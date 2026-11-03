(** Configuration for running expect tests *)

module Upon_unreleasable_issue = struct
  type t =
    [ `CR (** Leaves a CR, so that features cannot be released. *)
    | `CR_for_child (** Leaves a CR for [$child], to be resolved in a child feature. *)
    | `Warning_for_collector_testing (** Only for ppx_expect testing; do not use. *)
    ]
end

module Trailing_output_syntax = struct
  type t =
    [ `Extension (** Add trailing-output corrections as a [[%expect]] extension. *)
    | `Attribute
      (** Add trailing-output corrections as a [[@@expect.trailing]] attribute. *)
    ]
end

module type S = sig
  module IO : sig
    type 'a t

    val return : 'a -> 'a t
  end

  (** Run an IO operation until completion *)
  val run : (unit -> unit IO.t) -> unit

  (** [sanitize] can be used to map all output strings, e.g. for cleansing. *)
  val sanitize : string -> string

  (** [upon_unreleasable_issue] specifies how to report problematic output (e.g.
      backtraces or inconsistent outputs across runs). The default is [`CR]. *)
  val upon_unreleasable_issue : Upon_unreleasable_issue.t

  (** [trailing_output_syntax] controls how corrections for trailing output are inserted.

      [`Extension] is more familiar, but only reliably works if ['a IO.t = 'a].

      [`Attribute] works for arbitrary types, where it won't compile to just add
      [; [%expect {| |}]]. *)
  val trailing_output_syntax : Trailing_output_syntax.t
end
