open! Base
open Ppxlib
open Ast_builder.Default
open Ppx_expect_runtime_types [@@alert "-ppx_expect_runtime_types"]

let strict_indent = ref false
let allow_skipping_reachability_check = ref false

let allow_skipping_reachability_flag =
  "-expect-test-allow-output-block-to-suppress-reachability-check"
;;

module Expr = struct
  let option ~loc expression_of_a = function
    | Some x -> [%expr Some [%e expression_of_a ~loc x]]
    | None -> [%expr None]
  ;;

  let pair ~loc expression_of_a expression_of_b (a, b) =
    [%expr [%e expression_of_a ~loc a], [%e expression_of_b ~loc b]]
  ;;

  let delimiter ~loc (delimiter : String_node_format.Delimiter.t) =
    [%expr
      ([%e
         match delimiter with
         | T Quote -> [%expr T Quote]
         | T (Tag tag) -> [%expr T (Tag [%e estring ~loc tag])]]
       : (Ppx_expect_runtime.Delimiter.t[@alert "-ppx_expect_runtime"]))]
  ;;

  let id ~loc id =
    [%expr
      (Ppx_expect_runtime.Expectation_id.of_int_exn [@alert "-ppx_expect_runtime"])
        [%e eint ~loc (Expectation_id.to_int_exn id)]]
  ;;

  let compact_loc ~loc ({ start_bol; start_pos; end_pos } : Compact_loc.t) =
    [%expr
      { start_bol = [%e eint ~loc start_bol]
      ; start_pos = [%e eint ~loc start_pos]
      ; end_pos = [%e eint ~loc end_pos]
      }]
  ;;

  let payload ~loc ({ contents; tag } : Payload.t) =
    [%expr { contents = [%e estring ~loc contents]; tag = [%e delimiter ~loc tag] }]
  ;;

  let id_expr_alist ~loc alist =
    List.map alist ~f:(fun (expect_id, expr) -> [%expr [%e id ~loc expect_id], [%e expr]])
    |> elist ~loc
  ;;

  let flexibility_of_strictness ~loc =
    if !strict_indent
    then
      [%expr
        Ppx_expect_runtime.Expect_node_formatting.Flexibility.Exactly_formatted
        [@alert "-ppx_expect_runtime"]]
    else
      [%expr
        Ppx_expect_runtime.Expect_node_formatting.Flexibility.Flexible_modulo
          Ppx_expect_runtime.Expect_node_formatting.default
        [@alert "-ppx_expect_runtime"]]
  ;;
end

let compact_loc_of_ppxlib_location { loc_start; loc_end; loc_ghost = _ } : Compact_loc.t =
  { start_bol = loc_start.pos_bol
  ; start_pos = loc_start.pos_cnum
  ; end_pos = loc_end.pos_cnum
  }
;;

module Expectation_node = struct
  type expect_node_info =
    { located_payload : (Payload.t * Compact_loc.t) option
    ; node_loc : Compact_loc.t
    }

  type t =
    | Expect of expect_node_info
    | Expect_exact of expect_node_info
    | Expect_if_reached of expect_node_info
    | Expect_unreachable of Compact_loc.t

  let id t ~loc =
    match t with
    | Expect _ ->
      [%expr Ppx_expect_runtime.Test_node.Create.expect [@alert "-ppx_expect_runtime"]]
    | Expect_exact _ ->
      [%expr
        Ppx_expect_runtime.Test_node.Create.expect_exact [@alert "-ppx_expect_runtime"]]
    | Expect_if_reached _ ->
      [%expr
        Ppx_expect_runtime.Test_node.Create.expect_if_reached
        [@alert "-ppx_expect_runtime"]]
    | Expect_unreachable _ ->
      [%expr
        Ppx_expect_runtime.Test_node.Create.expect_unreachable
        [@alert "-ppx_expect_runtime"]]
  ;;

  let to_expr ~loc t =
    let make_expect_node node_expr { located_payload; node_loc } =
      [%expr
        [%e node_expr]
          ~formatting_flexibility:[%e Expr.flexibility_of_strictness ~loc]
          ~located_payload:
            [%e Expr.(option ~loc (pair payload compact_loc)) located_payload]
          ~node_loc:[%e Expr.compact_loc ~loc node_loc]]
    in
    match t with
    | Expect expect_node_info -> make_expect_node (id t ~loc) expect_node_info
    | Expect_exact expect_node_info -> make_expect_node (id t ~loc) expect_node_info
    | Expect_if_reached expect_node_info ->
      if !allow_skipping_reachability_check
      then make_expect_node (id t ~loc) expect_node_info
      else (
        let error_text =
          Printf.sprintf
            "ppx driver is not configured to accept [[%%expect.if_reached]] (can be \
             enabled by adding [%s] to ppx driver flags)"
            allow_skipping_reachability_flag
        in
        [%expr [%ocaml.error [%e estring ~loc error_text]]])
    | Expect_unreachable node_loc ->
      [%expr [%e id t ~loc] ~node_loc:[%e Expr.compact_loc ~loc node_loc]]
  ;;
end

module Pattern = struct
  open Ast_pattern

  let string () =
    map
      (single_expr_payload (as__ (pexp_constant (pconst_string __ __ __))))
      ~f:(fun f payload_expr contents _loc tag ->
        let (tag : String_node_format.Delimiter.t) =
          match tag with
          | None -> T Quote
          | Some tag -> T (Tag tag)
        in
        let payload_loc = compact_loc_of_ppxlib_location payload_expr.pexp_loc in
        let located_payload = Some (({ contents; tag } : Payload.t), payload_loc) in
        f ~located_payload)
  ;;

  let empty () = pstr nil
  let maybe_string () = string () ||| map (empty ()) ~f:(fun f -> f ~located_payload:None)
end

let maybe_string_payload = Pattern.maybe_string

module Parsed_node = struct
  type t =
    | Expectation_node of Expectation_id.t * Expectation_node.t
    | Output

  open struct
    let declare_expect name constructor =
      Extension.Expert.declare
        name
        Expression
        (Pattern.maybe_string ())
        (fun ~located_payload node_loc ->
           Expectation_node
             ( Expectation_id.lookup_or_mint Parsed node_loc
             , constructor located_payload node_loc ))
    ;;
  end

  let expect =
    declare_expect "expect" (fun located_payload node_loc ->
      Expect { located_payload; node_loc })
  ;;

  let expect_exact =
    declare_expect "expect_exact" (fun located_payload node_loc ->
      Expect_exact { located_payload; node_loc })
  ;;

  let expect_if_reached =
    declare_expect "expect.if_reached" (fun located_payload node_loc ->
      Expect_if_reached { located_payload; node_loc })
  ;;

  let expect_output =
    Extension.Expert.declare "@expect.output" Expression (Pattern.empty ()) (fun _ ->
      Output)
  ;;

  let expect_unreachable =
    Extension.Expert.declare
      "@expect.unreachable"
      Expression
      (Pattern.empty ())
      (fun compact_loc ->
         Expectation_node
           ( Expectation_id.lookup_or_mint Parsed compact_loc
           , Expect_unreachable compact_loc ))
  ;;

  let match_expectation =
    let expectations =
      [ expect; expect_exact; expect_if_reached; expect_output; expect_unreachable ]
    in
    function
    | { pexp_desc = Pexp_extension extension; pexp_loc; _ } ->
      Extension.Expert.convert expectations ~loc:pexp_loc extension
    | _ -> None
  ;;
end

let is_a_ppx_expect_ext_node e = Option.is_some (Parsed_node.match_expectation e)

let replace_and_collect_expects =
  object
    inherit [(Expectation_id.t, expression) List.Assoc.t] Ast_traverse.fold_map as super

    method! expression ({ pexp_attributes; pexp_loc = loc; _ } as expr) acc =
      match Parsed_node.match_expectation expr with
      | None -> super#expression expr acc
      | Some expect_node ->
        let expr, acc =
          match expect_node (compact_loc_of_ppxlib_location loc) with
          | Expectation_node (id, expect_expr) ->
            ( [%expr Ppx_expect_test_block.run_test ~test_id:[%e Expr.id ~loc id]]
            , (id, Expectation_node.to_expr expect_expr ~loc) :: acc )
          | Output ->
            [%expr Ppx_expect_test_block.read_test_output_no_backtrace_check ()], acc
        in
        Merlin_helpers.hide_expression { expr with pexp_attributes }, acc
  end
;;

let transform_let_expect
  ~trailing_location
  ~tags
  ~expected_exn
  ~description
  ~loc
  ~attrs
  body
  =
  let body, expectations = replace_and_collect_expects#expression body [] in
  let body_thunk =
    (* Copy extra attributes from the [let%expect_test] node to the [fun () -> body] thunk
       passed to [run_suite]. This is the correct behavior for at least the
       [[@@zero_alloc]] attribute, though likely any other similarly placed attribute as
       well. *)
    { ([%expr fun () -> [%e body]]) with pexp_attributes = attrs }
  in
  let filename_rel_to_project_root =
    Ppx_here_expander.expand_filename loc.loc_start.pos_fname
  in
  let trailing_location = compact_loc_of_ppxlib_location trailing_location in
  let body_loc =
    compact_loc_of_ppxlib_location
      { loc_start = loc.loc_start; loc_end = body.pexp_loc.loc_end; loc_ghost = true }
  in
  let trailing_test_id = Expectation_id.lookup_or_mint Trailing body_loc in
  let exn_test_id = Expectation_id.lookup_or_mint Exception body_loc in
  [%expr
    match Ppx_inline_test_lib.testing with
    | `Not_testing -> ()
    | `Testing _ ->
      if Ppx_inline_test_lib.force_drop
      then ()
      else
        let module Ppx_expect_test_block =
          Ppx_expect_runtime.Make_test_block [@alert "-ppx_expect_runtime"]
            (Expect_test_config)
        in
        Ppx_expect_test_block.run_suite
          ~filename_rel_to_project_root:[%e estring ~loc filename_rel_to_project_root]
          ~line_number:[%e eint ~loc loc.loc_start.pos_lnum]
          ~location:[%e Expr.compact_loc ~loc (compact_loc_of_ppxlib_location loc)]
          ~trailing_loc:[%e Expr.compact_loc ~loc trailing_location]
          ~body_loc:[%e Expr.compact_loc ~loc body_loc]
          ~formatting_flexibility:[%e Expr.flexibility_of_strictness ~loc]
          ~expected_exn:[%e Expr.(option ~loc (pair payload compact_loc)) expected_exn]
          ~trailing_test_id:[%e Expr.id ~loc trailing_test_id]
          ~exn_test_id:[%e Expr.id ~loc exn_test_id]
          ~description:[%e Expr.option estring ~loc description]
          ~tags:[%e tags |> List.map ~f:(estring ~loc) |> elist ~loc]
          ~inline_test_config:(module Inline_test_config)
          ~expectations:
            [%e Merlin_helpers.hide_expression (Expr.id_expr_alist ~loc expectations)]
          [%e body_thunk]]
;;

let let_expect_pat =
  let open Ast_pattern in
  let uncaught_exn =
    Attribute.declare_with_attr_loc
      "@expect.uncaught_exn"
      Attribute.Context.value_binding
      (Pattern.string ())
      (fun ~attr_loc ~located_payload -> attr_loc, located_payload)
  in
  let opt_name =
    map (pstring __) ~f:(fun f x -> f ~name:(Some x))
    ||| map ppat_any ~f:(fun f -> f ~name:None)
  in
  pstr
    (pstr_value
       nonrecursive
       (Attribute.pattern
          uncaught_exn
          (value_binding_attributes
             __
             (value_binding
                ~pat:
                  (map
                     (Attribute.pattern Ppx_inline_test.tags opt_name)
                     ~f:(fun f attributes ->
                       f ~tags:(Option.value ~default:[] attributes)))
                ~expr:__))
        ^:: nil)
     ^:: nil)
;;

let expect_test =
  Extension.V3.declare_inline
    "expect_test"
    Structure_item
    let_expect_pat
    (fun ~ctxt trailing attrs ~tags ~name code ->
       let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
       let loc = { loc with loc_ghost = true } in
       let trailing_location, expected_exn =
         match trailing with
         | Some (attr_loc, expected_exn) -> attr_loc, expected_exn
         | None -> { loc with loc_start = loc.loc_end }, None
       in
       Ppx_inline_test.validate_extension_point_exn
         ~name_of_ppx_rewriter:"ppx_expect"
         ~loc
         ~tags;
       transform_let_expect
         ~trailing_location
         ~tags
         ~expected_exn
         ~description:name
         ~loc
         ~attrs
         code
       |> Ppx_inline_test.maybe_drop loc)
;;

let () =
  Driver.add_arg
    "-expect-test-strict-indentation"
    (Bool (( := ) strict_indent))
    ~doc:
      (Printf.sprintf
         "BOOL Require standardized indentation in [[%%expect]] (default: %b)"
         !strict_indent);
  Driver.add_arg
    allow_skipping_reachability_flag
    (Bool (( := ) allow_skipping_reachability_check))
    ~doc:
      (Printf.sprintf
         "BOOL permit the [[%%expect.if_reached]] extension (default: %b)"
         !allow_skipping_reachability_check)
;;

let () =
  Driver.register_transformation
    "expect_test"
    ~rules:[ Context_free.Rule.extension expect_test ]
    ~enclose_impl:(fun source_file_loc ->
      match source_file_loc, Ppx_inline_test_libname.get () with
      | Some loc, Some _ ->
        (* Insert the header and footer used for "current file" tracking only if:
           1. The file is nonempty and
           2. The executable is being built with the [-inline-test-lib _] flag, indicating
           that there is some library for which we might run expect tests. If the
           [-inline-test-lib] flag was not passed, then we shouldn't insert the header and
           footer, as we will not be running expect tests and the [Ppx_expect_runtime]
           library might not even be in scope (as is the case in toplevel expect tests,
           which are not run through [Ppx_inline_test_lib]).
        *)
        let loc = { loc with loc_ghost = true } in
        let filename_rel_to_project_root =
          Ppx_here_expander.expand_filename loc.loc_start.pos_fname
        in
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          Ppx_inline_test.maybe_drop
            loc
            [%expr
              (Ppx_expect_runtime.Current_file.set [@alert "-ppx_expect_runtime"])
                ~filename_rel_to_project_root:
                  [%e estring ~loc filename_rel_to_project_root]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          Ppx_inline_test.maybe_drop
            loc
            [%expr
              (Ppx_expect_runtime.Current_file.unset [@alert "-ppx_expect_runtime"]) ()]
        in
        header, footer
      | _ -> [], [])
;;
