open! Base
open Types
include Payload_intf.Payload_types

let default contents = { contents; tag = String_node_format.Delimiter.default }

module Result = struct
  include Result

  let compare compare a b =
    match a, b with
    | Pass, Pass -> 0
    | Pass, _ -> -1
    | _, Pass -> 1
    | Fail a, Fail b -> compare a b
  ;;
end

module Payload_of (Contents : Contents) : Type = struct
  module Contents = Contents

  type 'a payload = 'a t
  type t = Contents.t payload

  let of_located_payload ~loc { contents; tag } =
    { contents = Contents.of_located_string ~loc contents; tag }
  ;;

  let to_source_code_string
    :  expect_node_formatting:Expect_node_formatting.t
    -> node_shape:String_node_format.Shape.t option -> indent:int option -> t -> string
    =
    fun ~expect_node_formatting ~node_shape ~indent t ->
    let rec fix_tag_conflicts test_output tag =
      let bad_tag tag fstr =
        String.is_substring ~substring:(Printf.sprintf fstr tag) test_output
      in
      if bad_tag tag "{%s|" || bad_tag tag "|%s}"
      then fix_tag_conflicts test_output (tag ^ "xxx")
      else tag
    in
    let escape_lines test_output =
      test_output
      |> String.split ~on:'\n'
      |> List.map ~f:String.escaped
      |> String.concat ~sep:"\n"
    in
    let tag, extension =
      match node_shape with
      | None -> t.tag, None
      | Some (T shape) ->
        let delimiter = String_node_format.Delimiter.handed t.tag shape.hand in
        T delimiter, Some (String_node_format.T { shape; delimiter })
    in
    let printed =
      Contents.to_source_code_string ~indent ~expect_node_formatting ~tag:t.tag t.contents
    in
    let payload_string () =
      match tag with
      | T (Tag tag) ->
        let tag = fix_tag_conflicts printed tag in
        Printf.sprintf "{%s|%s|%s}" tag printed tag
      | T Quote -> Printf.sprintf {|"%s"|} (escape_lines printed)
    in
    match extension with
    | None -> payload_string ()
    | Some (T { shape; delimiter }) ->
      (match shape.hand with
       | Longhand ->
         let prefix =
           match shape.kind with
           | Extension -> expect_node_formatting.extension_sigil
           | Attribute -> expect_node_formatting.attribute_sigil
         in
         Printf.sprintf "[%s%s %s]" prefix shape.name (payload_string ())
       | Shorthand ->
         let prefix =
           match shape.kind with
           | Extension -> expect_node_formatting.extension_sigil
         in
         (match delimiter with
          | Tag "" -> Printf.sprintf "{%s%s|%s|}" prefix shape.name printed
          | Tag tag -> Printf.sprintf "{%s%s %s|%s|%s}" prefix shape.name tag printed tag))
  ;;
end

module Pretty = Payload_of (struct
  let ws_only line = String.for_all line ~f:Char.is_whitespace

  let count_leading_spaces line =
    line |> String.to_list |> List.take_while ~f:(Char.( = ) ' ') |> List.length
  ;;

  module Line = struct
    type t =
      | Blank
      | Conflict_marker of string
      | Not_blank of int * string

    let compare a b =
      match a, b with
      | Blank, Blank -> 0
      | Blank, _ -> -1
      | _, Blank -> 1
      | Conflict_marker a, Conflict_marker b -> compare_string a b
      | Conflict_marker _, _ -> -1
      | _, Conflict_marker _ -> 1
      | Not_blank (a_indent, a_contents), Not_blank (b_indent, b_contents) ->
        (match compare_int a_indent b_indent with
         | 0 -> compare_string a_contents b_contents
         | n -> n)
    ;;

    let map_offset ~f = function
      | Not_blank (offset, s) -> Not_blank (f offset, s)
      | line -> line
    ;;

    let of_string line =
      if ws_only line
      then Blank
      else (
        let line_stripped = String.strip line in
        let is_conflict_marker =
          String.equal line_stripped "======="
          || List.exists [ "<<<<<<< "; "||||||| "; ">>>>>>> " ] ~f:(fun prefix ->
               String.is_prefix line_stripped ~prefix)
        in
        if is_conflict_marker
        then Conflict_marker line_stripped
        else (
          let leading = count_leading_spaces line in
          Not_blank (leading, line_stripped)))
    ;;

    let to_string ~initial_offset = function
      | Blank -> ""
      | Conflict_marker s -> s
      | Not_blank (offset, s) -> String.make (initial_offset + offset) ' ' ^ s
    ;;
  end

  type t =
    { leading_lines : int (** Number of empty lines at the start of the payload *)
    ; loc : Compact_loc.t option
        (** Location of the AST node associated with the [[%expect]] test containing this
          payload. [None] if this payload was not parsed from the source file, e.g.
          because it is constructed from the actual output collected at runtime *)
    ; body_lines : Line.t list
        (** Contents of the payload, excluding leading blank lines and trailing whitespace *)
    ; trailing_lines : int (** Number of empty lines at the end of the payload *)
    ; initial_offset : int option
        (** Number of additional spaces indenting each line of the payload *)
    ; final_offset : int (** Number of spaces following the last line of the payload *)
    ; already_formatted : bool
        (** Whether this payload is considered "canonicalized" and should resist further
          formatting (needed for toplevel tests) *)
    }

  let compare t1 t2 = List.compare Line.compare t1.body_lines t2.body_lines

  let test_passes ~expected_output ~test_output =
    (* Multi-line expectations must start with empty lines. *)
    (List.length expected_output.body_lines <= 1 || expected_output.leading_lines > 0)
    (* The contents and relative indentations of all lines in the outputs must be the
         same. *)
    && compare expected_output test_output = 0
  ;;

  let of_located_string ~loc output =
    let leading_lines, rest, final_offset =
      let lines = String.split_lines output in
      let final_offset =
        match List.last lines with
        | None -> 0
        | Some line -> String.rev line |> count_leading_spaces
      in
      let leading, rest = List.split_while ~f:ws_only lines in
      List.length leading, rest, final_offset
    in
    let trailing_lines, body_unstripped =
      let trailing_rev, body_rev = List.split_while ~f:ws_only (List.rev rest) in
      List.length trailing_rev, List.rev body_rev
    in
    let body_lines = List.map body_unstripped ~f:Line.of_string in
    let initial_offset =
      List.fold body_lines ~init:None ~f:(fun min_offset line ->
        match line, min_offset with
        | Not_blank (offset, _), None -> Some offset
        | Not_blank (offset, _), Some min_offset -> Some (Int.min offset min_offset)
        | _ -> min_offset)
    in
    let leading_lines, trailing_lines =
      (* If the body was completely blank, we want to count all of the blank lines as
           trailing, rather than leading (this is to preserve legacy behavior). *)
      match body_lines with
      | [] -> 0, Stdlib.max 0 (leading_lines - 1)
      (* We subtract one in order to preserve legacy behavior about trailing whitespace. *)
      | _ -> leading_lines, trailing_lines
    in
    let body_lines =
      let initial_offset = Option.value ~default:0 initial_offset in
      List.map body_lines ~f:(Line.map_offset ~f:(fun x -> x - initial_offset))
    in
    { leading_lines
    ; loc
    ; body_lines
    ; trailing_lines
    ; initial_offset
    ; final_offset
    ; already_formatted = false
    }
  ;;

  (* Format the [test_output] using the same indentation and leading/trailing whitespace
       decisions as [expected_output], using reasonable defaults if [expected_output] is
       [None]. *)
  let apply_format
    ~(expect_node_formatting : Expect_node_formatting.t)
    ~expected_output
    ~test_output
    =
    if test_output.already_formatted
    then test_output
    else (
      let output_size { body_lines; _ } =
        match body_lines with
        | [] -> `Empty
        | [ _ ] -> `Single
        | _ :: _ -> `Multi
      in
      let test_output_size = output_size test_output in
      match test_output_size with
      | `Empty ->
        (* The legacy behavior is not to respect any formatting decisions of the orginal
             expect node when a test fails with empty test_output. *)
        test_output
      | (`Single | `Multi) as test_output_size ->
        let default_offset =
          let { loc; _ } = Option.value expected_output ~default:test_output in
          (match loc with
           | Some { start_bol; start_pos; _ } -> start_pos - start_bol
           | None -> 0)
          + expect_node_formatting.indent
        in
        (match expected_output with
         | None ->
           { leading_lines =
               (match test_output_size with
                | `Multi -> 1
                | `Single -> 0)
           ; loc = test_output.loc
           ; body_lines = test_output.body_lines
           ; trailing_lines = 0
           ; initial_offset = Some default_offset
           ; final_offset = 0
           ; already_formatted = true
           }
         | Some ({ loc; _ } as expected_output) ->
           let expected_output_size = output_size expected_output in
           let leading_lines =
             match expected_output_size, test_output_size with
             | `Empty, `Single -> 0
             | `Empty, `Multi -> 1
             | (`Single | `Multi), `Single -> expected_output.leading_lines
             | (`Single | `Multi), `Multi -> max 1 expected_output.leading_lines
           in
           let initial_offset =
             match expected_output_size, expected_output.initial_offset with
             | `Empty, _ | _, None -> default_offset
             | (`Single | `Multi), Some expected_initial_offset ->
               (* The legacy behavior is to respect the indentation of the original
                    expect block exactly when both the original expectation and the test
                    output we plan to write span multiple lines, excluding trailing blank
                    lines. Notably this is different than both having size [`Multi] here,
                    because this includes leading blank lines. *)
               if Bool.equal
                    (List.length expected_output.body_lines
                     + expected_output.leading_lines
                     > 1)
                    (List.length test_output.body_lines + leading_lines > 1)
               then expected_initial_offset
               else default_offset
           in
           let trailing_lines, final_offset =
             match output_size expected_output, output_size test_output with
             | `Empty, `Single -> 0, 0
             | _, _ -> expected_output.trailing_lines, expected_output.final_offset
           in
           { leading_lines
           ; loc
           ; body_lines = test_output.body_lines
           ; trailing_lines
           ; initial_offset = Some initial_offset
           ; final_offset
           ; already_formatted = true
           }))
  ;;

  let reconcile ~expect_node_formatting ~expected_output ~test_output : _ Result.t =
    if test_passes ~expected_output ~test_output
    then Pass
    else
      Fail
        (apply_format
           ~expect_node_formatting
           ~expected_output:(Some expected_output)
           ~test_output)
  ;;

  let to_source_code_string ~expect_node_formatting ~indent ~tag contents =
    let ({ indent = formatting_indent
         ; always_on_own_line
         ; extension_sigil = _
         ; attribute_sigil = _
         }
          : Expect_node_formatting.t)
      =
      expect_node_formatting
    in
    let { leading_lines
        ; body_lines
        ; trailing_lines
        ; initial_offset
        ; final_offset
        ; loc = _
        ; already_formatted = _
        }
      =
      apply_format ~expect_node_formatting ~expected_output:None ~test_output:contents
    in
    let min_line_padding = if always_on_own_line then 1 else 0 in
    let leading_lines = Int.max leading_lines min_line_padding in
    let trailing_lines = Int.max trailing_lines min_line_padding in
    let initial_offset =
      match indent, initial_offset with
      | Some indent, _ -> indent + formatting_indent
      | None, Some initial_offset -> initial_offset
      | None, None -> 0
    in
    let tag_spacing =
      match (tag : String_node_format.Delimiter.t) with
      | T (Tag _) -> " " (* Delimited strings get padding like [{x| this |x}]. *)
      | T Quote -> "" (* Quoted strings get no padding like ["this"]. *)
    in
    match leading_lines, body_lines, trailing_lines with
    | _, [], _ ->
      (* An empty body should either be [{| |}] or [""] *)
      tag_spacing
    | 0, [ output ], 0 ->
      (* Fit on a single line *)
      tag_spacing ^ Line.to_string ~initial_offset:0 output ^ tag_spacing
    | _, _, _ ->
      (* Use multi-line string *)
      let body =
        body_lines
        |> List.map ~f:(Line.to_string ~initial_offset)
        |> String.concat ~sep:"\n"
      in
      let edge offset lines =
        (* Wrapping whitespace should be at least a newline or [tag_spacing] spaces *)
        match offset, lines with
        | 0, 0 -> tag_spacing
        | _ -> String.make lines '\n'
      in
      String.concat
        [ edge initial_offset leading_lines
        ; body
        ; edge final_offset trailing_lines
        ; String.make final_offset ' '
        ]
  ;;
end)

module Exact = Payload_of (struct
  include String

  let reconcile ~expect_node_formatting:_ ~expected_output ~test_output : _ Result.t =
    if equal expected_output test_output then Pass else Fail test_output
  ;;

  let of_located_string ~loc:_ t = t
  let to_source_code_string ~expect_node_formatting:_ ~indent:_ ~tag:_ contents = contents
end)
