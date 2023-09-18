(** Configuration for running expect tests *)

module Upon_unreleasable_issue = struct
  type t =
    [ `CR (** Leaves a CR, so that features cannot be released. *)
    | `Warning_for_collector_testing (** Only for ppx_expect testing; do not use. *)
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

  (** [upon_unreleasable_issue] specifies how to deal with output that should not be
      released even if it is accepted (e.g. backtraces). The default is [`CR].  *)
  val upon_unreleasable_issue : Upon_unreleasable_issue.t
end
