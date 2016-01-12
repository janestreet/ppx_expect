open StdLabels
open Sexplib.Std

module Pretty = struct
  module Line = struct
    type format =
      | Regexp  of string
      | Glob    of string
      | Literal of string
      [@@deriving sexp_of, variants, compare]

    type t =
      { as_string : string
      ; format    : format
      } [@@deriving sexp_of, compare]

    let empty =
      { as_string = ""
      ; format    = Literal ""
      }
  end

  type t =
    { leader  : string
    ; trailer : string
    ; indent  : string
    ; lines   : Line.t list
    } [@@deriving sexp_of, compare]

  let empty =
    { leader  = ""
    ; trailer = ""
    ; indent  = ""
    ; lines   = [Line.empty];
    }

  let to_string t =
    let body =
      let lines =
        match t.lines with
        | [] -> []
        | x::xs ->
          let indent : Line.t -> string = function
            | { as_string = ""; _ } -> ""
            | { as_string     ; _ } -> t.indent ^ as_string
          in
          let xs = List.map xs ~f:indent in
          if String.contains t.leader '\n'
          then indent x    :: xs
          else x.as_string :: xs
      in
      String.concat ~sep:"\n" lines
    in
    t.leader ^ body ^ t.trailer
  ;;
end

module Body = struct
  type kind = Exact | Pretty

  type t =
    | Exact  of string
    | Pretty of Pretty.t
    [@@deriving sexp_of, compare]
end

type t =
  { as_string : string
  ; tag       : string option
  ; body      : Body.t
  } [@@deriving sexp_of, compare]

