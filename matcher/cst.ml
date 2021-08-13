open! Base
open Import

let for_all_string s ~f =
  let b = ref true in
  for i = 0 to String.length s - 1 do
    b := !b && f s.[i]
  done;
  !b
;;

let is_blank = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let is_blanks s = for_all_string s ~f:is_blank
let is_spaces s = for_all_string s ~f:is_space
let no_nl s = for_all_string s ~f:(fun c -> Char.( <> ) c '\n')
let has_nl s = not (no_nl s)

module Line = struct
  type 'a not_blank =
    { trailing_blanks : string
    ; orig : string
    ; data : 'a
    }
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a not_blank) -> ()

  let sexp_of_not_blank : 'a. ('a -> Sexplib0.Sexp.t) -> 'a not_blank -> Sexplib0.Sexp.t =
    fun _of_a { trailing_blanks = v_trailing_blanks; orig = v_orig; data = v_data } ->
    let bnds = [] in
    let bnds =
      let arg = _of_a v_data in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "data"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_orig in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "orig"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_trailing_blanks in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_blanks"; arg ] :: bnds
    in
    Sexplib0.Sexp.List bnds
  ;;

  let _ = sexp_of_not_blank

  let compare_not_blank : 'a. ('a -> 'a -> int) -> 'a not_blank -> 'a not_blank -> int =
    fun _cmp__a a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match compare_string a__001_.trailing_blanks b__002_.trailing_blanks with
      | 0 ->
        (match compare_string a__001_.orig b__002_.orig with
         | 0 -> _cmp__a a__001_.data b__002_.data
         | n -> n)
      | n -> n)
  ;;

  let _ = compare_not_blank

  let equal_not_blank : 'a. ('a -> 'a -> bool) -> 'a not_blank -> 'a not_blank -> bool =
    fun _cmp__a a__003_ b__004_ ->
    if Ppx_compare_lib.phys_equal a__003_ b__004_
    then true
    else
      Ppx_compare_lib.( && )
        (equal_string a__003_.trailing_blanks b__004_.trailing_blanks)
        (Ppx_compare_lib.( && )
           (equal_string a__003_.orig b__004_.orig)
           (_cmp__a a__003_.data b__004_.data))
  ;;

  let _ = equal_not_blank

  [@@@end]

  type 'a t =
    | Blank of string
    | Not_blank of 'a not_blank
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : type a. (a -> Sexplib0.Sexp.t) -> a t -> Sexplib0.Sexp.t =
    fun _of_a -> function
      | Blank arg0__005_ ->
        let res0__006_ = sexp_of_string arg0__005_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Blank"; res0__006_ ]
      | Not_blank arg0__007_ ->
        let res0__008_ = sexp_of_not_blank _of_a arg0__007_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Not_blank"; res0__008_ ]
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__009_ b__010_ ->
    if Ppx_compare_lib.phys_equal a__009_ b__010_
    then 0
    else (
      match a__009_, b__010_ with
      | Blank _a__011_, Blank _b__012_ -> compare_string _a__011_ _b__012_
      | Blank _, _ -> -1
      | _, Blank _ -> 1
      | Not_blank _a__013_, Not_blank _b__014_ ->
        compare_not_blank _cmp__a _a__013_ _b__014_)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__017_ b__018_ ->
    if Ppx_compare_lib.phys_equal a__017_ b__018_
    then true
    else (
      match a__017_, b__018_ with
      | Blank _a__019_, Blank _b__020_ -> equal_string _a__019_ _b__020_
      | Blank _, _ -> false
      | _, Blank _ -> false
      | Not_blank _a__021_, Not_blank _b__022_ ->
        equal_not_blank _cmp__a _a__021_ _b__022_)
  ;;

  let _ = equal

  [@@@end]

  let map t ~f =
    match t with
    | Blank b -> Blank b
    | Not_blank n -> Not_blank { n with data = f n.orig n.data }
  ;;

  let strip = function
    | Blank _ -> Blank ""
    | Not_blank n -> Not_blank { n with trailing_blanks = "" }
  ;;

  let invariant inv = function
    | Blank s -> assert (is_blanks s)
    | Not_blank n ->
      assert (is_blanks n.trailing_blanks);
      inv n.data;
      assert (no_nl n.orig);
      let len = String.length n.orig in
      assert (len > 0 && not (is_blank n.orig.[len - 1]))
  ;;

  let data t ~blank =
    match t with
    | Blank _ -> blank
    | Not_blank n -> n.data
  ;;

  let orig = function
    | Blank _ -> ""
    | Not_blank n -> n.orig
  ;;
end

type 'a single_line =
  { leading_blanks : string
  ; trailing_spaces : string
  ; orig : string
  ; data : 'a
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a single_line) -> ()

let sexp_of_single_line : 'a. ('a -> Sexplib0.Sexp.t) -> 'a single_line -> Sexplib0.Sexp.t
  =
  fun _of_a
    { leading_blanks = v_leading_blanks
    ; trailing_spaces = v_trailing_spaces
    ; orig = v_orig
    ; data = v_data
    } ->
    let bnds = [] in
    let bnds =
      let arg = _of_a v_data in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "data"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_orig in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "orig"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_trailing_spaces in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_spaces"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_leading_blanks in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "leading_blanks"; arg ] :: bnds
    in
    Sexplib0.Sexp.List bnds
;;

let _ = sexp_of_single_line

let compare_single_line : 'a. ('a -> 'a -> int) -> 'a single_line -> 'a single_line -> int
  =
  fun _cmp__a a__025_ b__026_ ->
  if Ppx_compare_lib.phys_equal a__025_ b__026_
  then 0
  else (
    match compare_string a__025_.leading_blanks b__026_.leading_blanks with
    | 0 ->
      (match compare_string a__025_.trailing_spaces b__026_.trailing_spaces with
       | 0 ->
         (match compare_string a__025_.orig b__026_.orig with
          | 0 -> _cmp__a a__025_.data b__026_.data
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_single_line

let equal_single_line : 'a. ('a -> 'a -> bool) -> 'a single_line -> 'a single_line -> bool
  =
  fun _cmp__a a__027_ b__028_ ->
  if Ppx_compare_lib.phys_equal a__027_ b__028_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__027_.leading_blanks b__028_.leading_blanks)
      (Ppx_compare_lib.( && )
         (equal_string a__027_.trailing_spaces b__028_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__027_.orig b__028_.orig)
            (_cmp__a a__027_.data b__028_.data)))
;;

let _ = equal_single_line

[@@@end]

type 'a multi_lines =
  { leading_spaces : string
  ; trailing_spaces : string
  ; indentation : string
  ; lines : 'a Line.t list
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a multi_lines) -> ()

let sexp_of_multi_lines : 'a. ('a -> Sexplib0.Sexp.t) -> 'a multi_lines -> Sexplib0.Sexp.t
  =
  fun _of_a
    { leading_spaces = v_leading_spaces
    ; trailing_spaces = v_trailing_spaces
    ; indentation = v_indentation
    ; lines = v_lines
    } ->
    let bnds = [] in
    let bnds =
      let arg = sexp_of_list (Line.sexp_of_t _of_a) v_lines in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "lines"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_indentation in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "indentation"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_trailing_spaces in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "trailing_spaces"; arg ] :: bnds
    in
    let bnds =
      let arg = sexp_of_string v_leading_spaces in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "leading_spaces"; arg ] :: bnds
    in
    Sexplib0.Sexp.List bnds
;;

let _ = sexp_of_multi_lines

let compare_multi_lines : 'a. ('a -> 'a -> int) -> 'a multi_lines -> 'a multi_lines -> int
  =
  fun _cmp__a a__029_ b__030_ ->
  if Ppx_compare_lib.phys_equal a__029_ b__030_
  then 0
  else (
    match compare_string a__029_.leading_spaces b__030_.leading_spaces with
    | 0 ->
      (match compare_string a__029_.trailing_spaces b__030_.trailing_spaces with
       | 0 ->
         (match compare_string a__029_.indentation b__030_.indentation with
          | 0 ->
            compare_list
              (fun a__031_ b__032_ -> Line.compare _cmp__a a__031_ b__032_)
              a__029_.lines
              b__030_.lines
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_multi_lines

let equal_multi_lines : 'a. ('a -> 'a -> bool) -> 'a multi_lines -> 'a multi_lines -> bool
  =
  fun _cmp__a a__035_ b__036_ ->
  if Ppx_compare_lib.phys_equal a__035_ b__036_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__035_.leading_spaces b__036_.leading_spaces)
      (Ppx_compare_lib.( && )
         (equal_string a__035_.trailing_spaces b__036_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__035_.indentation b__036_.indentation)
            (equal_list
               (fun a__037_ b__038_ -> Line.equal _cmp__a a__037_ b__038_)
               a__035_.lines
               b__036_.lines)))
;;

let _ = equal_multi_lines

[@@@end]

type 'a t =
  | Empty of string
  | Single_line of 'a single_line
  | Multi_lines of 'a multi_lines
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a t) -> ()

let sexp_of_t : type a. (a -> Sexplib0.Sexp.t) -> a t -> Sexplib0.Sexp.t =
  fun _of_a -> function
    | Empty arg0__041_ ->
      let res0__042_ = sexp_of_string arg0__041_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Empty"; res0__042_ ]
    | Single_line arg0__043_ ->
      let res0__044_ = sexp_of_single_line _of_a arg0__043_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Single_line"; res0__044_ ]
    | Multi_lines arg0__045_ ->
      let res0__046_ = sexp_of_multi_lines _of_a arg0__045_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Multi_lines"; res0__046_ ]
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__047_ b__048_ ->
  if Ppx_compare_lib.phys_equal a__047_ b__048_
  then 0
  else (
    match a__047_, b__048_ with
    | Empty _a__049_, Empty _b__050_ -> compare_string _a__049_ _b__050_
    | Empty _, _ -> -1
    | _, Empty _ -> 1
    | Single_line _a__051_, Single_line _b__052_ ->
      compare_single_line _cmp__a _a__051_ _b__052_
    | Single_line _, _ -> -1
    | _, Single_line _ -> 1
    | Multi_lines _a__055_, Multi_lines _b__056_ ->
      compare_multi_lines _cmp__a _a__055_ _b__056_)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__059_ b__060_ ->
  if Ppx_compare_lib.phys_equal a__059_ b__060_
  then true
  else (
    match a__059_, b__060_ with
    | Empty _a__061_, Empty _b__062_ -> equal_string _a__061_ _b__062_
    | Empty _, _ -> false
    | _, Empty _ -> false
    | Single_line _a__063_, Single_line _b__064_ ->
      equal_single_line _cmp__a _a__063_ _b__064_
    | Single_line _, _ -> false
    | _, Single_line _ -> false
    | Multi_lines _a__067_, Multi_lines _b__068_ ->
      equal_multi_lines _cmp__a _a__067_ _b__068_)
;;

let _ = equal

[@@@end]

let invariant inv t =
  match t with
  | Empty s -> assert (is_spaces s)
  | Single_line s ->
    assert (is_blanks s.leading_blanks);
    assert (is_spaces s.trailing_spaces);
    inv s.data;
    assert (no_nl s.orig);
    let len = String.length s.orig in
    assert (len > 0 && (not (is_blank s.orig.[0])) && not (is_blank s.orig.[len - 1]))
  | Multi_lines m ->
    assert (is_spaces m.leading_spaces);
    let ld_len = String.length m.leading_spaces in
    assert (ld_len = 0 || Char.equal m.leading_spaces.[ld_len - 1] '\n');
    let tr_has_nl = has_nl m.trailing_spaces in
    assert (
      is_spaces m.trailing_spaces
      && ((not tr_has_nl) || Char.equal m.trailing_spaces.[0] '\n'));
    assert (is_blanks m.indentation);
    List.iter m.lines ~f:(Line.invariant inv);
    (match m.lines with
     | [] -> assert false
     | Blank _ :: _ -> assert false
     | [ Not_blank n ] ->
       assert (ld_len > 0 && (tr_has_nl || String.is_empty n.trailing_blanks))
     | l ->
       let rec check_last = function
         | [] -> assert false
         | [ Line.Blank _ ] -> assert false
         | [ Line.Not_blank n ] -> assert (tr_has_nl || String.is_empty n.trailing_blanks)
         | _ :: l -> check_last l
       in
       check_last l)
;;

let empty = Empty ""

let map t ~f =
  match t with
  | Empty e -> Empty e
  | Single_line s -> Single_line { s with data = f s.orig s.data }
  | Multi_lines m -> Multi_lines { m with lines = List.map m.lines ~f:(Line.map ~f) }
;;

let data t ~blank =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.data ]
  | Multi_lines m -> List.map m.lines ~f:(Line.data ~blank)
;;

let stripped_original_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.orig ]
  | Multi_lines m -> List.map m.lines ~f:Line.orig
;;

let line_of_single s : _ Line.t =
  Not_blank { trailing_blanks = ""; orig = s.orig; data = s.data }
;;

let to_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ line_of_single s ]
  | Multi_lines m -> m.lines
;;

let strip t =
  match t with
  | Empty _ -> Empty ""
  | Single_line s -> Single_line { s with leading_blanks = ""; trailing_spaces = "" }
  | Multi_lines m ->
    (match m.lines with
     | [] -> Empty ""
     | [ Blank _ ] -> assert false
     | [ Not_blank n ] ->
       Single_line
         { leading_blanks = ""; trailing_spaces = ""; orig = n.orig; data = n.data }
     | lines ->
       Multi_lines
         { leading_spaces = ""
         ; trailing_spaces = ""
         ; indentation = ""
         ; lines = List.map lines ~f:Line.strip
         })
;;

let to_string t =
  match t with
  | Empty s -> s
  | Single_line s -> s.leading_blanks ^ s.orig ^ s.trailing_spaces
  | Multi_lines m ->
    let indent (line : _ Line.t) =
      match line with
      | Blank b -> b
      | Not_blank n -> m.indentation ^ n.orig ^ n.trailing_blanks
    in
    let s = List.map m.lines ~f:indent |> String.concat ~sep:"\n" in
    m.leading_spaces ^ s ^ m.trailing_spaces
;;

let trim_lines lines =
  let rec loop0 : _ Line.t list -> _ = function
    | Blank _ :: l -> loop0 l
    | l -> loop1 l ~acc:[] ~acc_with_trailing_blanks:[]
  and loop1 ~acc ~acc_with_trailing_blanks = function
    | (Blank _ as x) :: l ->
      loop1 l ~acc ~acc_with_trailing_blanks:(x :: acc_with_trailing_blanks)
    | (Not_blank _ as x) :: l ->
      let acc = x :: acc_with_trailing_blanks in
      loop1 l ~acc ~acc_with_trailing_blanks:acc
    | [] -> List.rev acc
  in
  loop0 lines
;;

let not_blank_lines lines =
  List.fold_left lines ~init:[] ~f:(fun acc (l : _ Line.t) ->
    match l with
    | Blank _ -> acc
    | Not_blank n -> n.orig :: acc)
  |> List.rev
;;

let longest_common_prefix a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let len = min len_a len_b in
  let i = ref 0 in
  while !i < len && Char.equal a.[!i] b.[!i] do
    Int.incr i
  done;
  String.sub a ~pos:0 ~len:!i
;;

let indentation s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_blank s.[!i] do
    Int.incr i
  done;
  String.sub s ~pos:0 ~len:!i
;;

let extract_indentation lines =
  match not_blank_lines lines with
  | [] -> "", lines
  | first :: rest ->
    let indent = List.fold_left rest ~init:(indentation first) ~f:longest_common_prefix in
    let indent_len = String.length indent in
    let update_line : 'a Line.t -> 'a Line.t = function
      | Blank b -> Blank b
      | Not_blank n ->
        let orig =
          String.sub n.orig ~pos:indent_len ~len:(String.length n.orig - indent_len)
        in
        Not_blank { n with orig }
    in
    indent, List.map lines ~f:update_line
;;

let break s at = String.prefix s at, String.drop_prefix s at

let reconcile (type a) t ~lines ~default_indentation ~pad_single_line =
  let module M = struct
    type t =
      | Empty
      | Single_line of a Line.not_blank
      | Multi_lines of a Line.t list
  end
  in
  let lines =
    match trim_lines lines |> extract_indentation |> snd with
    | [] -> M.Empty
    | [ Blank _ ] -> assert false
    | [ Not_blank n ] -> M.Single_line n
    | lines -> M.Multi_lines lines
  in
  let padding = if pad_single_line then " " else "" in
  let res =
    match t, lines with
    | Empty _, Empty -> t
    | Single_line s, Single_line n -> Single_line { s with orig = n.orig; data = n.data }
    | Multi_lines m, Multi_lines l -> Multi_lines { m with lines = l }
    | Empty e, Multi_lines l ->
      let ld, tr =
        if has_nl e
        then (
          let ld, tr = break e (String.index_exn e '\n') in
          ld ^ "\n", tr)
        else "\n", padding
      in
      Multi_lines
        { leading_spaces = ld
        ; trailing_spaces = tr
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line m, Multi_lines l ->
      Multi_lines
        { leading_spaces = "\n"
        ; trailing_spaces = m.trailing_spaces
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line _, Empty | Multi_lines _, Empty -> Empty padding
    | Empty _, Single_line n ->
      Single_line
        { orig = n.orig
        ; data = n.data
        ; leading_blanks = padding
        ; trailing_spaces = padding
        }
    | Multi_lines m, Single_line n -> Multi_lines { m with lines = [ Not_blank n ] }
  in
  invariant ignore res;
  res
;;
