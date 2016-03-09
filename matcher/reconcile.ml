open Expect_test_common.Std
open StdLabels
open Sexplib.Std
open Ocaml_re

module Result = struct
  (* Either match with an explicit success, or (lazily) produce a correction. *)
  type 'a t =
    | Match
    | Correction of 'a
  [@@deriving sexp_of, compare]

  let map t ~f =
    match t with
    | Match -> Match
    | Correction x -> Correction (f x)
  ;;

  let value t ~success =
    match t with
    | Match     -> success
    | Correction f -> f
  ;;
end

let matches_regexp ~(pat : Re.t) s =
  Re.execp (Re.compile (Re.whole_string pat)) s
;;

external matches_glob : glob:string -> string -> bool = "ppx_expect_matches_glob"

let line_matches ~(expect : Fmt.t) ~actual =
  match expect with
  | Literal expect ->
    expect = actual
  | Glob expect ->
    matches_glob ~glob:expect actual
  | Regexp expect ->
    matches_regexp ~pat:(Re_emacs.re expect) actual
;;

let%test _ = line_matches ~expect:(Literal "foo") ~actual:"foo"
let%test _ = line_matches ~expect:(Literal "f.*o (regexp)") ~actual:"f.*o (regexp)"

let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"
let%test _ = not (line_matches ~expect:(Regexp "f.*o") ~actual:"foo (regexp)")
let%test _ = not (line_matches ~expect:(Regexp "[a]") ~actual:"[a]")
let%test _ = line_matches ~expect:(Regexp "f.*o") ~actual:"foo"

(* Regexp provides the possibility to match trailing *)
let%test _ = line_matches ~expect:(Regexp "f.*o[ ]") ~actual:"foo "

let literal_line actual : Fmt.t Cst.Line.t =
  match actual with
  | "" -> Blank ""
  | _  ->
    let line_matches_itself =
      line_matches ~expect:(Lexer.parse_pretty_line actual) ~actual
    in
    Not_blank
      { data = Literal actual
      ; orig = if line_matches_itself then actual else actual ^ " (literal)"
      ; trailing_blanks = ""
      }
;;

let reconcile_line ~(expect:Fmt.t) ~actual
  : Fmt.t Cst.Line.t Result.t =
  assert (not (String.contains actual '\n'));
  if line_matches ~expect ~actual
  then Match
  else Correction (literal_line actual)
;;

let%test_module _ =
  (module struct
    let expect_match ~expect ~actual =
      let expect = Lexer.parse_pretty_line expect in
      [%test_result: Fmt.t Cst.Line.t Result.t]
        (reconcile_line ~expect ~actual)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~corrected =
      let expect = Lexer.parse_pretty_line expect in
      let corrected : Fmt.t Cst.Line.t =
        Not_blank
          { orig = corrected
          ; data = Lexer.parse_pretty_line corrected
          ; trailing_blanks = "" }
      in
      [%test_result: Fmt.t Cst.Line.t Result.t]
        (reconcile_line ~expect ~actual)
        ~expect:(Correction corrected)
    ;;

    let%test_unit _ =
      expect_match
        ~expect:"foo"
        ~actual:"foo"

    let%test_unit _ =
      expect_match
        ~expect:"[a] (regexp)"
        ~actual:"a"

    let%test_unit _ =
      expect_correction
        ~expect:"[a] (regexp)"
        ~actual:"b"
        ~corrected:"b"
  end)

let rec reconcile_lines
    ~(expect_lines : Fmt.t Cst.Line.t list)
    ~(actual_lines : string list)
    : Fmt.t Cst.Line.t list Result.t =
  match expect_lines, actual_lines with
  | [], [] -> Match
  | [], actual_lines ->
    Correction (List.map actual_lines ~f:literal_line)
  | _, [] -> Correction []
  | (expect::expect_lines), (actual::actual_lines) ->
    let format = Cst.Line.data expect ~blank:(Literal "") in
    let line = reconcile_line ~expect:format ~actual in
    let rest = reconcile_lines ~expect_lines ~actual_lines in
    match line, rest with
    | Match, Match -> Match
    | _ ->
      Correction (
        let line = Result.value line ~success:expect in
        line :: Result.value rest ~success:expect_lines
      )
;;

let expectation_body_internal
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
  : Fmt.t Cst.t Expectation.Body.t Result.t =
  match expect with
  | Exact expect ->
    if expect = actual
    then Match
    else Correction (Exact actual)
  | Pretty expect ->
    let actual_lines =
      Lexer.strip_surrounding_whitespaces actual
      |> Cst.stripped_original_lines
    in
    let expect_lines = Cst.to_lines expect in
    match reconcile_lines ~expect_lines ~actual_lines with
    | Match -> Match
    | Correction reconciled_lines ->
      let reconciled =
        Cst.reconcile expect
          ~lines:reconciled_lines
          ~default_indentation:default_indent
          ~pad_single_line
      in
      Correction (Pretty reconciled)
;;

let expectation_body
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
  : Fmt.t Cst.t Expectation.Body.t Result.t =
  let res = expectation_body_internal ~expect ~actual ~default_indent ~pad_single_line in
  match res with
  | Match -> Match
  | Correction c ->
    match
      expectation_body_internal ~expect:c ~actual ~default_indent ~pad_single_line
    with
    | Match -> res
    | Correction _ ->
      assert false
;;

let%test_module _ =
  (module struct
    let strip s =
      Lexer.strip_surrounding_whitespaces s
    ;;

    let nb orig trailing_blanks =
      Cst.Line.Not_blank { orig; trailing_blanks; data = () }
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip "\n  ")
        ~expect:(Empty "\n  ")
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip "   \n\
               \   foo   \n\
               \     bar     \n\
               \     plop  \n\
               \  \n\
               \    blah \n\
               \ \n\
               \   ")
        ~expect:(Multi_lines
                   { leading_spaces  = "   \n"
                   ; trailing_spaces = "\n \n   "
                   ; indentation     = "   "
                   ; lines =
                       [ nb    "foo"     "   "
                       ; nb    "  bar"   "     "
                       ; nb    "  plop"  "  "
                       ; Blank "  "
                       ; nb    " blah"   " "
                       ]
                   })
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip "abc \n\
                def ")
        ~expect:(Multi_lines
                   { leading_spaces  = ""
                   ; trailing_spaces = " "
                   ; indentation     = ""
                   ; lines =
                       [ nb    "abc" " "
                       ; nb    "def" ""
                       ]
                   })
    ;;

    let%test_unit _ =
      [%test_result: unit Cst.t]
        (strip " [a] (regexp) ")
        ~expect:(Single_line
                   { leading_blanks  = " "
                   ; trailing_spaces = " "
                   ; orig            = "[a] (regexp)"
                   ; data            = ()
                   })
    ;;

    let expect_match ~expect ~actual =
      let expect = Lexer.parse_body (Pretty expect) in
      [%test_result: Fmt.t Cst.t Expectation.Body.t Result.t]
        (expectation_body ~expect ~actual ~default_indent:0 ~pad_single_line:true)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~default_indent ~corrected =
      let expect = Lexer.parse_body (Pretty expect) in
      [%test_result: Fmt.t Cst.t Expectation.Body.t Result.t]
        (expectation_body ~expect ~actual ~default_indent ~pad_single_line:true)
        ~expect:(Correction corrected)
    ;;

    let%test_unit _ =
      expect_match
        ~expect:" foo "
        ~actual:"foo"

    let%test_unit _ =
      expect_match
        ~expect:"foo\n\
                 [a] (regexp)"
        ~actual:"foo\na"

    let%test_unit _ =
      expect_correction
        ~expect:"foo\n\
                 [a] (regexp)"
        ~actual:"foo\nb"
        ~default_indent:0
        ~corrected:(
          Lexer.parse_body
            (Pretty
               "foo\n\
                b")
        )

    (* check regexp are preserved in corrections *)
    let%test_unit _ =
      expect_correction
        ~expect:"foo\n\
                 [ab]* (regexp)"
        ~actual:"not-foo\nbaba"
        ~default_indent:0
        ~corrected:(
          Lexer.parse_body
            (Pretty
               "not-foo\n\
                [ab]* (regexp)")
        )

  end)
