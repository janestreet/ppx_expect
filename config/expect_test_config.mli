(** Configuration for running expect tests *)

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

module type S = sig
  (** IO monad *)
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  (** Flush whatever need to be to get pending output out on file descriptor 0. *)
  val flush : unit -> unit IO.t

  (** Run an IO operation until completion *)
  val run : (unit -> unit IO.t) -> unit

  (** Synchronous check that there is no pending output on file description 0. With async,
      there is no guarantee that on the rhs of a [IO.bind (flush ()) ...] the output is
      completely flushed, that's why we need this. *)
  val flushed : unit -> bool
end

include S with type 'a IO.t = 'a
