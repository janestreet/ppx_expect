open! Base
open Import
open Ppx_compare_lib.Builtin
open Sexplib0.Sexp_conv

module Body = struct
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : type a. (a -> Sexplib0.Sexp.t) -> a t -> Sexplib0.Sexp.t =
    fun _of_a -> function
      | Exact arg0__001_ ->
        let res0__002_ = sexp_of_string arg0__001_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Exact"; res0__002_ ]
      | Output -> Sexplib0.Sexp.Atom "Output"
      | Pretty arg0__003_ ->
        let res0__004_ = _of_a arg0__003_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Pretty"; res0__004_ ]
      | Unreachable -> Sexplib0.Sexp.Atom "Unreachable"
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__005_ b__006_ ->
    if Ppx_compare_lib.phys_equal a__005_ b__006_
    then 0
    else (
      match a__005_, b__006_ with
      | Exact _a__007_, Exact _b__008_ -> compare_string _a__007_ _b__008_
      | Exact _, _ -> -1
      | _, Exact _ -> 1
      | Output, Output -> 0
      | Output, _ -> -1
      | _, Output -> 1
      | Pretty _a__009_, Pretty _b__010_ -> _cmp__a _a__009_ _b__010_
      | Pretty _, _ -> -1
      | _, Pretty _ -> 1
      | Unreachable, Unreachable -> 0)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__011_ b__012_ ->
    if Ppx_compare_lib.phys_equal a__011_ b__012_
    then true
    else (
      match a__011_, b__012_ with
      | Exact _a__013_, Exact _b__014_ -> equal_string _a__013_ _b__014_
      | Exact _, _ -> false
      | _, Exact _ -> false
      | Output, Output -> true
      | Output, _ -> false
      | _, Output -> false
      | Pretty _a__015_, Pretty _b__016_ -> _cmp__a _a__015_ _b__016_
      | Pretty _, _ -> false
      | _, Pretty _ -> false
      | Unreachable, Unreachable -> true)
  ;;

  let _ = equal

  [@@@end]

  let map_pretty t ~f =
    match t with
    | (Exact _ | Output | Unreachable) as t -> t
    | Pretty x -> Pretty (f x)
  ;;
end

type 'a t =
  { tag : string option
  ; body : 'a Body.t
  ; extid_location : File.Location.t
  ; body_location : File.Location.t
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a t) -> ()

let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a
    { tag = v_tag
    ; body = v_body
    ; extid_location = v_extid_location
    ; body_location = v_body_location
    } ->
    let bnds = [] in
    let bnds =
      let arg = File.Location.sexp_of_t v_body_location in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "body_location"; arg ] :: bnds
    in
    let bnds =
      let arg = File.Location.sexp_of_t v_extid_location in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "extid_location"; arg ] :: bnds
    in
    let bnds =
      let arg = Body.sexp_of_t _of_a v_body in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "body"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_option sexp_of_string v_tag in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "tag"; arg ] :: bnds
    in
    Sexplib0.Sexp.List bnds
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__017_ b__018_ ->
  if Ppx_compare_lib.phys_equal a__017_ b__018_
  then 0
  else (
    match compare_option compare_string a__017_.tag b__018_.tag with
    | 0 ->
      (match Body.compare _cmp__a a__017_.body b__018_.body with
       | 0 ->
         (match File.Location.compare a__017_.extid_location b__018_.extid_location with
          | 0 -> File.Location.compare a__017_.body_location b__018_.body_location
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__023_ b__024_ ->
  if Ppx_compare_lib.phys_equal a__023_ b__024_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_option equal_string a__023_.tag b__024_.tag)
      (Ppx_compare_lib.( && )
         (Body.equal _cmp__a a__023_.body b__024_.body)
         (Ppx_compare_lib.( && )
            (File.Location.equal a__023_.extid_location b__024_.extid_location)
            (File.Location.equal a__023_.body_location b__024_.body_location)))
;;

let _ = equal

[@@@end]

module Raw = struct
  type nonrec t = string t [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : t) -> ()
  let sexp_of_t = (fun x__029_ -> sexp_of_t sexp_of_string x__029_ : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  let compare =
    (fun a__030_ b__031_ -> compare compare_string a__030_ b__031_ : t -> t -> int)
  ;;

  let _ = compare

  [@@@end]
end

let map_pretty t ~f = { t with body = Body.map_pretty t.body ~f }
