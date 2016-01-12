
module Pretty : sig
  module Line : sig
    type format =
      | Regexp  of string
      | Glob    of string
      | Literal of string
      [@@deriving sexp_of, compare]

    type t =
      { as_string : string (* The entire original line, as it appears in the input *)
      ; format    : format (* The de-formatted string, strongly typed for the right
                              matcher *)
      } [@@deriving sexp_of, compare]

    val empty : t
  end

  (** An expect body with support for formatting and fancier matching *)
  type t =
    { leader  : string      (* Leading whitespace to be stripped from the first line *)
    ; trailer : string      (* Trailing whitespace to be stripped from the last line *)
    ; indent  : string      (* Leading whitespace to be stripped from every line     *)
    ; lines   : Line.t list
    } [@@deriving sexp_of, compare]

  val empty : t

  val to_string : t -> string
end

module Body : sig
  type kind = Exact | Pretty

  type t =
    | Exact  of string
    | Pretty of Pretty.t
    [@@deriving sexp_of, compare]
end

(**
  [body] contains the parsed body of an [%%expect BODY] syntax extension.
  [as_string] contains the original text including the opening "[%%expect" and the
  closing "]"
*)
type t =
  { as_string : string
  ; tag       : string option
  ; body      : Body.t
  } [@@deriving sexp_of, compare]
