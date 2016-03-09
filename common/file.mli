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

  val beginning_of_file : Name.t -> t

  module Map : sig
    include MoreLabels.Map.S with type key = t
    val of_alist : (key * 'a) list -> 'a t
  end
end

module Digest : sig
  type t [@@deriving sexp_of, compare]
  val of_string : string -> t
  val to_string : t -> string
end
