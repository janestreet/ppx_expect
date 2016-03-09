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
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Flush whatever need to be to get pending output out on file descriptor 0. *)
  val flush : unit -> unit IO.t

  (** Run an IO operation until completion *)
  val run : (unit -> unit IO.t) -> unit
end

include S with type 'a IO.t = 'a
