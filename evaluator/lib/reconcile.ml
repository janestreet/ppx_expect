open StdLabels
open Sexplib.Std
open Ocaml_re

let choose_tag ~default body =
  let terminators = Lexer.extract_quoted_string_terminators body in
  let rec loop tag =
    if List.mem tag ~set:terminators then
      loop (tag ^ "x")
    else
      tag
  in
  if List.mem default ~set:terminators then
    loop (if default = "" then "xxx" else default ^ "_xxx")
  else
    default
;;

let%test_module _ =
  (module struct
    let test body = [%test_result: string] (choose_tag ~default:"" body)

    let%test_unit _ = test "nice text"                                ~expect:""
    let%test_unit _ = test "with embedded |} somewhere"               ~expect:"xxx"
    let%test_unit _ = test "with embedded |a} somewhere"              ~expect:""
    let%test_unit _ = test "with embedded |xxx} somewhere"            ~expect:""
    let%test_unit _ = test "double - |} and |xxx} - embedding"        ~expect:"xxxx"

    let testD body = [%test_result: string] (choose_tag ~default:"default" body)

    let%test_unit _ = testD "nice text"                               ~expect:"default"
    let%test_unit _ = testD "with embedded |} somewhere"              ~expect:"default"
    let%test_unit _ = testD "with embedded |default} somewhere"       ~expect:"default_xxx"
    let%test_unit _ = testD "double - |default} and |default_xxx}"    ~expect:"default_xxxx"
  end)

(* Either match with an explicit success, or (lazily) produce a correction. *)
type 'a t =
  | Match
  | Correction of 'a
[@@deriving sexp_of, compare]

let value t ~success =
  match t with
  | Match     -> success
  | Correction f -> f
;;

let matches_regexp ~(pat : Re.t) s =
  Re.execp (Re.compile (Re.whole_string pat)) s
;;

external matches_glob : glob:string -> string -> bool = "ppx_expect_matches_glob"

let line_matches ~(expect : Expectation.Pretty.Line.format) ~actual =
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

let literal_line actual : Expectation.Pretty.Line.t =
  let expect = (Lexer.parse_pretty_line actual).format in
  (* The actual output may not match itself (e.g. if it ends with (regexp)). If
      that's the case, append a (literal) tag so that it will. *)
  { format    = Literal actual
  ; as_string =
      if line_matches ~expect ~actual
      then actual
      else actual ^ " (literal)"
  }
;;

let reconcile_line ~(expect:Expectation.Pretty.Line.format) ~(actual:string)
    : Expectation.Pretty.Line.t t =
  assert (not (String.contains actual '\n'));
  if line_matches ~expect ~actual
  then Match
  else Correction (literal_line actual)
;;

let%test_module _ =
  (module struct
    let expect_match ~expect ~actual =
      let expect = (Lexer.parse_pretty_line expect).format in
      [%test_result: Expectation.Pretty.Line.t t]
        (reconcile_line ~expect ~actual)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~corrected =
      let expect = (Lexer.parse_pretty_line expect).format in
      let corrected = Lexer.parse_pretty_line corrected in
      [%test_result: Expectation.Pretty.Line.t t]
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
    ~(expect_lines : Expectation.Pretty.Line.t list)
    ~(actual_lines: string list)
    : Expectation.Pretty.Line.t list t =
  match expect_lines, actual_lines with
  | [], [] -> Match
  | [], actual_lines ->
    Correction (List.map actual_lines ~f:literal_line)
  | _, [] -> Correction []
  | (expect::expect_lines), (actual::actual_lines) ->
    let line = reconcile_line ~expect:expect.format ~actual in
    let rest = reconcile_lines ~expect_lines ~actual_lines in
    match line, rest with
    | Match, Match -> Match
    | _ ->
      Correction (
        let line = value line ~success:expect in
        line :: value rest ~success:expect_lines
      )
;;

let expectation_body
      ~(expect : Expectation.Body.t)
      ~actual
      ~default_indent
  : Expectation.Body.t t =
  match expect with
  | Exact expect ->
    if expect = actual
    then Match
    else Correction (Exact actual)
  | Pretty expect ->
    let actual_lines = Lexer.split_lines actual in
    match reconcile_lines ~expect_lines:expect.lines ~actual_lines with
    | Match -> Match
    | Correction reconciled_lines ->
      Correction (
        let reconciled =
          let expect_lines = expect.lines in
          let expect = { expect with lines = reconciled_lines } in
          let expect =
            if List.length expect_lines > 1
            then expect
            else
            match actual_lines with
            | [] -> assert false
            | _ :: _ :: _ ->
              { expect with
                leader = "\n";
                indent = String.make (default_indent + 2) ' ';
                trailer =
                  (* Only apply an indent to the end if there's a trailing newline. *)
                  if List.hd (List.rev actual_lines) = ""
                  then String.make default_indent ' '
                  else "";
              }
            | [""] ->
              { expect with
                leader = "";
                trailer = "";
              }
            | [_] ->
              { expect with
                leader = " ";
                trailer = " ";
              }
          in
          Expectation.Pretty.to_string expect
        in
        let output = Lexer.parse_body reconciled ~kind:Pretty in
        output
      )
;;

let improved_expectation_body
      ~(expect : Expectation.Body.t)
      ~actual
      ~default_indent
  : Expectation.Body.t t =
  let res = expectation_body ~expect ~actual ~default_indent in
  match res with
  | Match -> Match
  | Correction c ->
    match expectation_body ~expect:c ~actual ~default_indent with
    | Match -> res
    | Correction _ -> Correction (Expectation.Body.Exact actual)

let expectation_body = improved_expectation_body

let%test_module _ =
  (module struct
    let expect_match ~expect ~actual =
      let expect = Lexer.parse_body expect ~kind:Pretty in
      [%test_result: Expectation.Body.t t]
        (expectation_body ~expect ~actual ~default_indent:0)
        ~expect:Match
    ;;

    let expect_correction ~expect ~actual ~default_indent ~corrected =
      let expect = Lexer.parse_body expect ~kind:Pretty in
      [%test_result: Expectation.Body.t t]
        (expectation_body ~expect ~actual ~default_indent)
        ~expect:(Correction corrected)
    ;;

    let%test_unit _ =
      expect_match
        ~expect:{| foo |}
        ~actual:"foo"

    let%test_unit _ =
      expect_match
        ~expect:{| foo
                  [a] (regexp)|}
        ~actual:"foo\na"

    let%test_unit _ =
      expect_correction
        ~expect:{| foo
                  [a] (regexp) |}
        ~actual:"foo\nb"
        ~default_indent:0
        ~corrected:(
          Lexer.parse_body ~kind:Pretty
                {| foo
                  b |}
        )

    (* check regexp are preserved in corrections *)
    let%test_unit _ =
      expect_correction
        ~expect:{| foo
                  [ab]* (regexp) |}
        ~actual:"not-foo\nbaba"
        ~default_indent:0
        ~corrected:(
          Lexer.parse_body ~kind:Pretty
                {| not-foo
                  [ab]* (regexp) |}
        )

  end)

let expectation
      ~(expect : Expectation.t)
      ~actual
      ~default_indent
  : Expectation.t t =
  match expectation_body ~expect:expect.body ~actual ~default_indent with
  | Match -> Match
  | Correction body ->
    Correction (
      let (name, body_string) =
        match body with
        | Exact expect -> ("expect_exact", expect)
        | Pretty expect -> ("expect", Expectation.Pretty.to_string expect)
      in
      match expect.tag with
      | None ->
        {
          as_string = Printf.sprintf "[%%%s \"%s\"]" name (String.escaped body_string);
          tag = None;
          body;
        }
      | Some default ->
        let tag = choose_tag ~default body_string in
        {
          as_string = Printf.sprintf "[%%%s {%s|%s|%s}]" name tag body_string tag;
          tag = Some tag;
          body;
        }
    )
;;
