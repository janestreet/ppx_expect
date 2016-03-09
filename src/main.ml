open Expect_test_common.Std
open StdLabels
open Ppx_core.Std
open Ast_builder.Default
open Parsetree

[@@@metaloc loc]

let lifter ~loc =
  let make_longident typ_name name : Longident.t Located.t =
    let id =
      match String.rindex typ_name '.' with
      | exception Not_found -> Lident name
      | i -> Longident.parse (String.sub typ_name ~pos:0 ~len:(i + 1) ^ name)
    in
    Located.mk ~loc id
  in object
  inherit [expression] Expectation_lifter.lifter
  method constr typ_name (name, args) =
    pexp_construct ~loc (make_longident typ_name name)
      (match args with
       | [] -> None
       | _  -> Some (pexp_tuple ~loc args))
  method string s = estring ~loc s
  method int x = eint ~loc x
  method record typ_name fields =
    pexp_record ~loc
      (List.map fields ~f:(fun (name, expr) ->
         (make_longident typ_name name, expr)))
      None
  method lift_Expect_test_common__File_Name_t file_name =
    eapply ~loc (evar ~loc "Expect_test_common.Std.File.Name.of_string")
      [ estring ~loc (File.Name.to_string file_name) ]
  [@@@ocaml.warning "-7"]
  method lift_Expect_test_common_File_Name_t file_name =
    eapply ~loc (evar ~loc "Expect_test_common.Std.File.Name.of_string")
      [ estring ~loc (File.Name.to_string file_name) ]
  [@@@ocaml.warning "+7"]
end

let lift_location ~loc of_loc =
  (lifter ~loc)#lift_Expect_test_common_File_Location_t of_loc
;;

let lift_expectation ~loc expect =
  (lifter ~loc)#lift_Expect_test_common_Std_Expectation_Raw_t expect
;;

let estring_option ~loc x =
  match x with
  | None -> pexp_construct ~loc (Located.mk ~loc (lident "None")) None
  | Some s ->
    pexp_construct ~loc (Located.mk ~loc (lident "Some")) (Some (estring ~loc s))
;;

(* Grab a list of all the output expressions *)
let collect_expectations = object
  inherit [(Location.t * Expectation.Raw.t) list] Ast_traverse.fold as super

  method! expression expr acc =
    match Expect_extension.match_expectation expr with
    | None ->
      super#expression expr acc
    | Some ext ->
      assert_no_attributes expr.pexp_attributes;
      (expr.pexp_loc, ext) :: acc
end

let replace_expects = object
  inherit [string] Ast_traverse.map_with_context as super

  method! expression instance_var ({ pexp_attributes; pexp_loc; _ } as expr) =
    match Expect_extension.match_expectation expr with
    | None -> super#expression instance_var expr
    | Some ext ->
      let loc = { pexp_loc with loc_end = pexp_loc.loc_start } in
      let expr =
        [%expr
          Expect_test_collector.Instance.save_output
            [%e evar ~loc instance_var]
            [%e lift_location ~loc ext.extid_location]
        ]
      in
      { expr with pexp_attributes }
end

let file_digest =
  let cache = Hashtbl.create 32 in
  fun fname ->
    match Hashtbl.find cache fname with
    | hash -> hash
    | exception Not_found ->
      let hash = Digest.file fname |> Digest.to_hex in
      Hashtbl.add cache fname hash;
      hash

let rewrite_test_body ~descr pstr_loc body =
  let loc = pstr_loc in
  let expectations =
    List.map (collect_expectations#expression body [])
      ~f:(fun (loc, expect_extension) -> lift_expectation ~loc expect_extension)
    |> elist ~loc
  in

  let instance_var = gen_symbol ~prefix:"_ppx_expect_instance" () in
  let body = replace_expects#expression instance_var body in

  let hash = file_digest loc.loc_start.pos_fname in
  [%expr
    let module Expect_test_collector = Expect_test_collector.Make(Expect_test_config) in
    Expect_test_collector.run
      ~file_digest:  (Expect_test_common.Std.File.Digest.of_string [%e estring ~loc hash])
      ~location:     [%e lift_location ~loc (Ppx_expect_payload.transl_loc pstr_loc)]
      ~description:  [%e estring_option ~loc descr]
      ~expectations: [%e expectations]
      ~inline_test_config:(module Inline_test_config)
      (fun [%p pvar ~loc instance_var] -> [%e body])
  ]

(* Recurse explicitly to ensure all [let %expect_test] are at toplevel. This is to ensure
   that they can run only once and only from the module where they are defined. *)
let rec rewrite_structure str =
  match str with
  | [] -> []
  | item :: str ->
    match Expect_extension.match_expect_test item with
    | Some (descr, body) ->
      let items =
        rewrite_test_body ~descr item.pstr_loc body
        |> Ppx_inline_test.maybe_drop item.pstr_loc
      in
      items @ rewrite_structure str
    | None ->
      let item =
        match item.pstr_desc with
        | Pstr_module mb ->
          let pmb_expr = rewrite_module_expr mb.pmb_expr in
          { item with pstr_desc = Pstr_module { mb with pmb_expr } }
        | _ ->
          item
      in
      item :: rewrite_structure str

and rewrite_module_expr me =
  match me.pmod_desc with
  | Pmod_structure str ->
    { me with pmod_desc = Pmod_structure (rewrite_structure str) }
  | Pmod_constraint (me, mt) ->
    { me with pmod_desc = Pmod_constraint (rewrite_module_expr me, mt) }
  | _ -> me

let check_leftover = object
  inherit Ast_traverse.iter as super

  method! structure_item item =
    match Expect_extension.match_expect_test item with
    | None -> super#structure_item item
    | Some _ ->
      Location.raise_errorf ~loc:item.pstr_loc
        "let%%expect_test is only allowed at the toplevel or in sub-modules"
end

let disabled = object
  inherit Ast_traverse.map as super

  method! structure_item item =
    match Expect_extension.match_expect_test item with
    | None -> super#structure_item item
    | Some _ ->
      Location.raise_errorf ~loc:item.pstr_loc
        "ppx_expect: extension is disabled as no -inline-test-lib was given"
end

let disabled = disabled#structure

let enabled ast =
  let ast = rewrite_structure ast in
  check_leftover#structure ast;
  ast
;;

let () =
  Ppx_driver.register_transformation "expect_test"
    ~impl:(fun ast ->
      match Ppx_inline_test_libname.get () with
      | None -> disabled ast
      | Some _ -> enabled ast
    )
;;
