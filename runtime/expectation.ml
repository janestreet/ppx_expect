open! Base
open Types
include Expectation_intf.Definitions

module Insert_loc = struct
  include Insert_loc

  let loc = function
    | Overwrite { whole_node; payload = _ } -> whole_node
    | Insert { loc; body_loc = _ } -> loc
  ;;
end

let with_behavior
  { position
  ; behavior = _
  ; payload_type
  ; on_incorrect_output
  ; inconsistent_outputs_message
  }
  behavior
  =
  { position; behavior; payload_type; on_incorrect_output; inconsistent_outputs_message }
;;

let formatter
  (type behavior)
  ~(expect_node_formatting : Expect_node_formatting.t)
  ({ position
   ; behavior
   ; payload_type
   ; on_incorrect_output = T on_incorrect_output
   ; inconsistent_outputs_message = _
   } :
    behavior t)
  =
  let count_leading_spaces line =
    line |> String.to_list |> List.take_while ~f:(Char.( = ) ' ') |> List.length
  in
  Output.Formatter.create
  @@
  match payload_type with
  | Exact -> Fn.id
  | Pretty ->
    fun str ->
      let lines =
        (* In pretty payloads, we normalize all newlines to ['\n']. [[%expect_exact ""]]
           can be used in cases where a user wants to inspect the whitespace produced by
           their output more closely. *)
        let stripped =
          str
          |> String.split_lines
          |> List.map ~f:(String.rstrip ~drop:Char.is_whitespace)
          |> List.drop_while ~f:String.is_empty
          |> List.rev
          |> List.drop_while ~f:String.is_empty
          |> List.rev
        in
        let indent_and_contents =
          List.map stripped ~f:(fun line ->
            (* The legacy behavior is to only count the longest prefix of actual spaces
               ([' ']) for indentation, but to strip all whitespace (including, e.g.,
               ['\t']). Note that this means [" \t contents"] is counted as having
               contents ["contents"] and indentation [1]. *)
            count_leading_spaces line, String.strip line)
        in
        match
          indent_and_contents
          |> List.filter_map ~f:(function
               | _indent, "" -> None
               | indent, _ -> Some indent)
          |> List.min_elt ~compare:Int.compare
        with
        | None -> []
        | Some min_indent ->
          List.map indent_and_contents ~f:(fun (indent, line) ->
            Int.max 0 (indent - min_indent), line)
      in
      let (T tag) =
        match (behavior : _ Behavior.t) with
        | Expect { payload = { tag; contents = _ }; _ } -> tag
        | Unreachable _ -> String_node_format.Delimiter.default
      in
      (match lines, expect_node_formatting.always_on_own_line with
       | [], _ ->
         (* An empty body should either be [{| |}] or [""] *)
         (match tag with
          | Tag _ -> " "
          | Quote -> "")
       | [ (_indent, line) ], false ->
         (* A single line should either be [{| line |}] or ["line"] *)
         (match tag with
          | Tag _ -> String.concat [ " "; line; " " ]
          | Quote -> line)
       | lines, _ ->
         let location_indent =
           expect_node_formatting.indent
           (* The contents are always indented two spaces past the left edge of the
              extension point *)
           +
           match position with
           | Overwrite { whole_node = { start_bol; start_pos; end_pos = _ }; payload = _ }
             ->
             (* If we are overwriting an extension point, we should take its left edge *)
             start_pos - start_bol
           | Insert { body_loc = { start_bol; start_pos; end_pos = _ }; loc = _ } ->
             (* If we are inserting a new extension point, we should compute its left
                edged from the left edge of the [let%expect_test] node *)
             start_pos
             - start_bol
             +
             (match on_incorrect_output.kind with
              | Extension -> expect_node_formatting.indent
              | Attribute -> 0)
         in
         let spaces n = String.make n ' ' in
         let first_line, indentation, last_line =
           match tag with
           | Quote ->
             (* Since ocamlformat will split the string onto lines and indent them for
                us, we shouldn't insert literal whitespace to indent the string. *)
             " ", 1, " "
           | Tag _ -> "", location_indent, spaces location_indent
         in
         let lines =
           List.map lines ~f:(function
             | _indent, "" -> ""
             | line_indent, line -> spaces (indentation + line_indent) ^ line)
         in
         [ [ first_line ]; lines; [ last_line ] ]
         |> List.concat
         |> String.concat ~sep:"\n")
;;

let extension_syntax extension_name ~payload_loc ~node_loc =
  let contains (outer : Compact_loc.t) ~(inner : Compact_loc.t) =
    outer.start_pos <= inner.start_pos && outer.end_pos >= inner.end_pos
  in
  match payload_loc with
  | Some payload_loc when contains payload_loc ~inner:node_loc ->
    (* An extension point whose payload location contains the location of the entire
       extension point is using the "shorthand" syntax. *)
    (T { name = extension_name; kind = Extension; hand = Shorthand }
      : String_node_format.Shape.t)
  | _ -> T { name = extension_name; kind = Extension; hand = Longhand }
;;

let possibly_relax_strictness
  ~(formatting_flexibility : Expect_node_formatting.Flexibility.t)
  (t : [ `Expect ] t)
  =
  match formatting_flexibility with
  | Exactly_formatted -> t
  | Flexible_modulo expect_node_formatting ->
    let fmt = formatter ~expect_node_formatting t in
    let (Expect { payload = { contents; tag }; on_unreachable; reachability }) =
      t.behavior
    in
    (match
       Output.reconcile
         ~expected_output:contents
         ~test_output:(Output.Formatter.apply fmt contents)
     with
     | Pass -> t
     | Fail contents ->
       let payload = Output.to_formatted_payload ~tag contents in
       with_behavior t (Expect { payload; on_unreachable; reachability }))
;;

let expected_string_and_payload_loc = function
  | Some (a, b) -> a, Some b
  | None -> Output.Payload.default "", None
;;

let expect ~formatting_flexibility ~node_loc ~located_payload =
  let payload, payload_loc = expected_string_and_payload_loc located_payload in
  { position = Overwrite { whole_node = node_loc; payload = payload_loc }
  ; behavior =
      Expect
        { payload; on_unreachable = Replace_with_unreachable; reachability = Can_reach }
  ; payload_type = Pretty
  ; on_incorrect_output = extension_syntax "expect" ~payload_loc ~node_loc
  ; inconsistent_outputs_message = "test output"
  }
  |> possibly_relax_strictness ~formatting_flexibility
;;

let expect_exact ~formatting_flexibility ~node_loc ~located_payload =
  let payload, payload_loc = expected_string_and_payload_loc located_payload in
  { position = Overwrite { whole_node = node_loc; payload = payload_loc }
  ; behavior =
      Expect
        { payload; on_unreachable = Replace_with_unreachable; reachability = Can_reach }
  ; payload_type = Exact
  ; on_incorrect_output = extension_syntax "expect_exact" ~payload_loc ~node_loc
  ; inconsistent_outputs_message = "test output"
  }
  |> possibly_relax_strictness ~formatting_flexibility
;;

let expect_unreachable ~node_loc =
  { position = Overwrite { whole_node = node_loc; payload = None }
  ; behavior = Unreachable { reachability_of_corrected = Can_reach }
  ; payload_type = Pretty
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_uncaught_exn ~formatting_flexibility ~node_loc ~located_payload =
  let payload, payload_loc = expected_string_and_payload_loc located_payload in
  { position = Overwrite { whole_node = node_loc; payload = payload_loc }
  ; behavior = Expect { payload; on_unreachable = Delete; reachability = Must_reach }
  ; payload_type = Pretty
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
  |> possibly_relax_strictness ~formatting_flexibility
;;

let expect_trailing ~insert_loc =
  { position = Insert insert_loc
  ; behavior =
      Expect
        { payload = Output.Payload.default " "
        ; on_unreachable = Silent
        ; reachability = Can_reach
        }
  ; payload_type = Pretty
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "trailing output"
  }
;;

let expect_no_uncaught_exn ~insert_loc =
  { position = Insert insert_loc
  ; behavior = Unreachable { reachability_of_corrected = Must_reach }
  ; payload_type = Pretty
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;

module For_apply_style = struct
  let format_payload mk_node =
    Staged.stage
    @@ fun ~expect_node_formatting ~payload_loc ~node_loc tag contents ->
    let node =
      mk_node
        ~formatting_flexibility:(Exactly_formatted : Expect_node_formatting.Flexibility.t)
        ~node_loc
        ~located_payload:(Some (({ tag; contents } : Output.Payload.t), payload_loc))
    in
    let formatted_contents =
      Output.Formatter.apply (formatter ~expect_node_formatting node) contents
    in
    match Output.reconcile ~expected_output:contents ~test_output:formatted_contents with
    | Pass -> None
    | Fail contents ->
      let source_code_string =
        match node.on_incorrect_output with
        | T { hand = Longhand; _ } ->
          Output.to_formatted_payload ~tag contents
          |> Output.Payload.to_source_code_string
        | T { hand = Shorthand; _ } as node_shape ->
          Output.to_source_code_string ~expect_node_formatting ~node_shape ~tag contents
      in
      Some source_code_string
  ;;

  let format_expect_payload = format_payload expect |> Staged.unstage
  let format_uncaught_exn_payload = format_payload expect_uncaught_exn |> Staged.unstage
end
