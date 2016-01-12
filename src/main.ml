open Ppx_core.Std
open Ast_builder.Default
open Parsetree

[@@@metaloc loc]

module Location = struct
  include Location

  let compare c1 c2 =
    compare c1.loc_start.pos_cnum c2.loc_start.pos_cnum
  ;;
end

module List = struct
  include ListLabels

  let filter_map l ~f =
    let rec loop accum = function
      | [] -> accum
      | x::xs ->
        match f x with
        | None -> loop accum xs
        | Some y -> loop (y :: accum) xs
    in
    rev (loop [] l)
  ;;
end

let lift_location ~loc (of_loc : Location.t) =
  [%expr
    { filename    = Expect_test_collector.File.Name.of_string
                      [%e estring ~loc loc.loc_start.pos_fname]
    ; line_start  = [%e eint ~loc of_loc.loc_start.pos_bol ]
    ; line_number = [%e eint ~loc of_loc.loc_start.pos_lnum]
    ; start_pos   = [%e eint ~loc of_loc.loc_start.pos_cnum]
    ; end_pos     = [%e eint ~loc of_loc.loc_end.pos_cnum  ]
    }
  ]
;;

let lift_expectation ~loc { Expect_extension. expected; tag; is_exact } =
  [%expr
    { expected = [%e estring ~loc expected]
    ; tag =
        [%e
          match tag with
          | None -> pexp_construct ~loc (Located.mk ~loc (lident "None")) None
          | Some s -> pexp_construct ~loc (Located.mk ~loc (lident "Some")) (Some (estring ~loc s))
        ]
    ; is_exact = [%e ebool   ~loc is_exact]
    }
  ]
;;

(* Grab a list of all the output expressions *)
let collect_expressions = object
  inherit [expression list] Ast_traverse.fold as super

  method! expression expr expressions =
    expr :: super#expression expr expressions
end

let replace_expects = object
  inherit [string] Ast_traverse.map_with_context as super

  method! expression instance_var ({ pexp_attributes; pexp_loc; _ } as expr) =
    match Expect_extension.match_expectation expr with
    | None -> super#expression instance_var expr
    | Some _ ->
      let loc = { pexp_loc with loc_end = pexp_loc.loc_start } in
      let expr =
        eapply ~loc (evar ~loc "Expect_test_collector.Instance.save_output")
          [ evar ~loc instance_var
          ; lift_location ~loc pexp_loc
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

let rewrite_test_body pstr_loc body =
  let loc = pstr_loc in
  let expressions = collect_expressions#expression body [] in
  let expectations =
    List.filter_map expressions
      ~f:(fun ({ pexp_loc; pexp_attributes; _ } as expr) ->
        match Expect_extension.match_expectation expr with
        | None -> None
        | Some expect_extension -> begin
            assert_no_attributes pexp_attributes;
            Some (
              pexp_tuple ~loc
                [ lift_location    ~loc pexp_loc
                ; lift_expectation ~loc expect_extension
                ]
            )
          end
      )
    |> fun expectations ->
    elist ~loc expectations
  in
  let default_indent =
    let { pexp_loc; _ } =
      expressions
      |> List.sort ~cmp:(fun x y -> Location.compare x.pexp_loc y.pexp_loc)
      |> List.hd
    in
    pexp_loc.loc_start.pos_cnum - pexp_loc.loc_start.pos_bol
  in

  let instance_var = gen_symbol ~prefix:"_ppx_expect_instance" () in
  let body = replace_expects#expression instance_var body in

  let hash = file_digest loc.loc_start.pos_fname in
  [%expr
    Expect_test_collector.run
      ~file_digest:   (Expect_test_collector.File.Digest.of_string [%e estring ~loc hash])
      ~location:      [%e lift_location ~loc pstr_loc]
      ~expectations:  [%e expectations]
      ~default_indent:[%e eint ~loc default_indent]
      (fun [%p pvar ~loc instance_var] -> [%e body])
  ]

(* Recurse explicitly to ensure all [let %expect_test] are at toplevel. This is to ensure
   that they can run only once and only from the module where they are defined. *)
let rec rewrite_structure str =
  match str with
  | [] -> []
  | item :: str ->
    match Expect_extension.match_expect_test item with
    | Some (_, body) ->
      let items =
        rewrite_test_body item.pstr_loc body
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
