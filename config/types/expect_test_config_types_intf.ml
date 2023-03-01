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

(** Configuration for running expect tests *)
module type Expect_test_config_types = sig
  (** To configure expect_test, add the following at the top of your .ml file, or in some
      import.ml:

      {[
        module Expect_test_config = struct
          include Expect_test_config
          let pre_redirect_hook () = ...
        end
      ]}

      Note that since all expect test are also inline tests, the inline test configuration
      also applies to all expect test.
  *)

  module Upon_unreleasable_issue : sig
    include module type of Upon_unreleasable_issue

    val equal : t -> t -> bool
    val comment_prefix : t -> string

    (** Message to print when an expectation contains a backtrace *)
    val message_when_expectation_contains_backtrace : t -> string
  end

  module type S = S
end
