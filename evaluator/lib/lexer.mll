{
  open StdLabels
  open Sexplib.Std
  open Expectation

  let escaped s =
    let unescaped = Scanf.unescaped s in
    if String.contains unescaped '\n'
    then
      failwith "(escaped) strings can't contain escaped newlines";
    Pretty.Line.Literal unescaped

  let%test_unit _ =
    [%test_result: string] (Scanf.unescaped "xx\\n\032yy") ~expect:"xx\n yy"

  let make_line as_string format : Pretty.Line.t = { format; as_string }

  let determine_indent lines (* list of (indent_length, full_line) *) =
    match List.filter lines ~f:((<>) (0, "")) with
    | [] -> ""
    | (n, s) :: l ->
      let indent_len = List.map l ~f:fst |> List.fold_left ~init:n ~f:min in
      let indent = String.sub s ~pos:0 ~len:indent_len in
      List.iter l ~f:(fun (_, s) ->
        if String.sub s ~pos:0 ~len:indent_len <> indent then
          failwith "Inconsistent indentation. Try checking tabs and spaces?");
      indent

  let process_lines lines =
    let indent = determine_indent lines in
    let indent_len = String.length indent in
    let unindent = function
      | "" -> ""
      | s  -> String.sub s ~pos:indent_len ~len:(String.length s - indent_len)
    in
    let lines = List.map lines ~f:snd |> List.map ~f:unindent in
    (indent, lines)

  type raw_pretty =
    { leader  : string
    ; trailer : string
    ; indent  : string
    ; lines   : string list
    }
}

let space = [' ' '\t']
let line_contents = (space* [^' ' '\t' '\n']+)*
let lowercase = ['a'-'z' '_']

rule line = parse
  | line_contents as s space* "(literal)" space* as all eof { make_line all (Literal s) }
  | line_contents as s space* "(escaped)" space* as all eof { make_line all (escaped s) }
  | line_contents as s space* "(regexp)"  space* as all eof { make_line all (Regexp  s) }
  | line_contents as s space* "(glob)"    space* as all eof { make_line all (Glob    s) }
  | line_contents as s space*                    as all eof { make_line all (Literal s) }

and split_and_process_lines acc = parse
  | space* as indent line_contents space* as line "\n"
    { split_and_process_lines ((String.length indent, line) :: acc) lexbuf }
  (* On the last line, trailing whitespaces are captured specially *)
  | space* as trailer eof
    { (trailer, process_lines (List.rev ((0, "") :: acc))) }
  | space* as indent line_contents as line (space* as trailer) eof
    { (trailer, process_lines (List.rev ((String.length indent, line) :: acc))) }

and raw_pretty = parse
  | space* as s eof
    { { leader = ""; trailer = ""; indent = ""; lines = [s] } }
  | (space* as leader) (line_contents as s) (space* as trailer) eof
    { { leader; trailer; indent = ""; lines = [s] } }
  | "\n"
    { let trailer, (indent, lines) = split_and_process_lines [] lexbuf in
      { leader = "\n"; trailer; indent; lines } }
  | (space* as leader) ([^'\n']* as first) "\n"
    { let trailer, (indent, lines) = split_and_process_lines [] lexbuf in
      { leader; trailer; indent; lines = first :: lines } }

and quoted_string_terminators acc = parse
  | "|" (lowercase* as s) "}" { quoted_string_terminators (s :: acc) lexbuf }
  | _                         { quoted_string_terminators       acc  lexbuf }
  | eof                       { acc }

and split_lines = parse
  | [^'\n']* as s eof  { s :: []                 }
  | [^'\n']* as s "\n" { s :: split_lines lexbuf }

{
  let parse_pretty_line s = line (Lexing.from_string s)

  let parse_pretty s =
    let { leader; trailer; indent; lines } = raw_pretty (Lexing.from_string s) in
    let res : Pretty.t =
      { leader
      ; trailer
      ; indent
      ; lines = List.map lines ~f:parse_pretty_line
      }
    in
    [%test_result: string] (Pretty.to_string res) ~expect:s;
    res

  let parse_body s ~(kind : Body.kind) : Body.t =
    match kind with
    | Exact  -> Exact s
    | Pretty -> Pretty (parse_pretty s)

  let extract_quoted_string_terminators s =
    quoted_string_terminators [] (Lexing.from_string s)

  let split_lines s = split_lines (Lexing.from_string s)
}
