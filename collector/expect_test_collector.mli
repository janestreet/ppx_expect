module Expectation : sig
  type t =
    { expected  : string
    ; tag       : string option
    ; is_exact  : bool
    }
end

module File : sig
  module Name : sig
    (** Strongly-typed filename *)
    type t [@@deriving sexp, compare]
    val to_string : t -> string
    val of_string : string -> t
  end

  module Location : sig
    (** Location within a file *)
    type t =
      { filename    : Name.t
      ; line_number : int
      ; line_start  : int
      ; start_pos   : int
      ; end_pos     : int
      } [@@deriving sexp, compare]
  end

  module Digest : sig
    type t [@@deriving sexp_of, compare]
    val of_string : string -> t
    val to_string : t -> string
  end
end

type t =
  { file_digest     : File.Digest.t
  ; location        : File.Location.t
  ; expectations    : (File.Location.t * Expectation.t) list
  ; saved_output    : (File.Location.t * string) list
  ; trailing_output : string
  ; default_indent  : int
  }

module Instance : sig
  type t

  (** Collect the output that has been run since the last call to [save_output], or since
      the current expect-test started running.

      This function should only be called while a test is running. It is meant to be
      called as a result of ppx_expect translating an expect-test, and is not intended to
      be called manually. *)
  val save_output : t -> File.Location.t -> unit
end

(** Run an expect-test *)
val run
  :  file_digest    : File.Digest.t
  -> location       : File.Location.t
  -> expectations   : (File.Location.t * Expectation.t) list
  -> default_indent : int
  -> (Instance.t -> unit)
  -> unit

val tests_run : unit -> t list
