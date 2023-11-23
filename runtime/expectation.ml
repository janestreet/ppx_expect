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

let loc { position; _ } = Insert_loc.loc position

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
  (type output behavior)
  ~(expect_node_formatting : Expect_node_formatting.t)
  ({ position
   ; behavior
   ; payload_type = _
   ; on_incorrect_output = T on_incorrect_output
   ; inconsistent_outputs_message = _
   } :
    (output, behavior) t)
  =
  Output.Formatter.create
  @@ fun str ->
  let lines =
    (* Whitespace splitting/stripping specifically targets ['\n'] and [' '] so that
       unusual characters like ['\r'] and ['\t'] get displayed explicitly in
       expectations. *)
    let stripped =
      str
      |> String.split ~on:'\n'
      |> List.map ~f:(String.rstrip ~drop:(Char.equal ' '))
      |> List.drop_while ~f:String.is_empty
      |> List.rev
      |> List.drop_while ~f:String.is_empty
      |> List.rev
    in
    let indent_and_contents =
      List.map stripped ~f:(fun line ->
        let unindented = String.lstrip ~drop:(Char.equal ' ') line in
        String.length line - String.length unindented, unindented)
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
  match lines, expect_node_formatting.always_on_own_line with
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
      | Overwrite { whole_node = { start_bol; start_pos; end_pos = _ }; payload = _ } ->
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
    [ [ first_line ]; lines; [ last_line ] ] |> List.concat |> String.concat ~sep:"\n"
;;

let extension_syntax extension_name ~payload_loc ~loc =
  let contains (outer : Compact_loc.t) ~(inner : Compact_loc.t) =
    outer.start_pos <= inner.start_pos && outer.end_pos >= inner.end_pos
  in
  match payload_loc with
  | Some payload_loc when contains payload_loc ~inner:loc ->
    (* An extension point whose payload location contains the location of the entire
       extension point is using the "shorthand" syntax. *)
    (T { name = extension_name; kind = Extension; hand = Shorthand }
      : String_node_format.Shape.t)
  | _ -> T { name = extension_name; kind = Extension; hand = Longhand }
;;

let expect ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Pretty.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = extension_syntax "expect" ~payload_loc ~loc
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_exact ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Exact.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Exact)
  ; on_incorrect_output = extension_syntax "expect_exact" ~payload_loc ~loc
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_unreachable loc =
  { position = Overwrite { whole_node = loc; payload = None }
  ; behavior = Unreachable { reachability_of_corrected = Can_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_uncaught_exn ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      Expect
        { payload = Payload.Pretty.of_located_payload ~loc:None payload
        ; on_unreachable = Delete
        ; reachability = Must_reach
        }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;

let expect_trailing virtual_loc =
  { position = Insert virtual_loc
  ; behavior =
      (let loc = Some virtual_loc.loc in
       Expect
         { payload = Payload.default (Payload.Pretty.Contents.of_located_string ~loc "")
         ; on_unreachable = Silent
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "trailing output"
  }
;;

let expect_no_uncaught_exn virtual_loc =
  { position = Insert virtual_loc
  ; behavior = Unreachable { reachability_of_corrected = Must_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;

module For_apply_style = struct
  let format_payload mk_node =
    Staged.stage
    @@ fun ~expect_node_formatting ~loc tag contents ->
    let formatted_contents =
      Output.Formatter.apply
        (formatter
           ~expect_node_formatting
           (mk_node ~payload_loc:None ({ tag; contents } : _ Payload.t) loc))
        contents
    in
    match Output.reconcile ~expected_output:contents ~test_output:formatted_contents with
    | Pass -> None
    | Fail contents ->
      Some
        (Output.to_source_code_string
           ~expect_node_formatting
           ~node_shape:None
           ~tag
           contents)
  ;;

  let format_expect_payload = format_payload expect |> Staged.unstage
  let format_uncaught_exn_payload = format_payload expect_uncaught_exn |> Staged.unstage
end
