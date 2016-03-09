(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

(* Temporary hacks *)
let js_hacks = function
  | After_rules ->
    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
            ]));

    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])
  | _ -> ()

let dispatch = function
  | After_rules ->
    rule "expectation lifter"
      ~prod:"src/expectation_lifter.ml"
      ~deps:["common/expectation.cmi"
            ;"common/file.cmi"
            ;"common/std.cmi" ]
      (fun _ _ ->
        Cmd (S [ P "ocamlfind"
               ; A "ppx_tools/genlifter"
               ; A "-I"
               ; A "common"
               ; A "Expect_test_common.Std.Expectation.Raw.t"
               ; Sh ">"
               ; A "src/expectation_lifter.ml"
               ]));

    (* Urgh. *)
    (* OCamlbuild is apparently lost with dependencies. *)
    List.iter (fun (ext, lib_ext, cmox) ->
      let ordered =
        ["common/expect_test_common" ^ lib_ext
        ;"expect_payload/ppx_expect_payload" ^ lib_ext
        ; "src/ppx_expect" ^ lib_ext
        ; "as_ppx/ppx" ^ cmox]
      in
      rule ("ugly workaround for ocamlbuild sad bugs " ^ ext)
        ~insert:`top
        ~prod:("as_ppx/ppx" ^ ext)
        ~deps:ordered
        (fun _ _ ->
          Cmd (S [ !Options.ocamlopt
                 ; A "-linkpkg"
                 ; A "-predicates"
                 ; A "ppx_driver"
                 ; A "-package"
                 ; A "ppx_tools.metaquot,ppx_driver,ppx_core,ppx_inline_test,\
                      ppx_inline_test.libname,sexplib"
                 ; S (List.map (fun s -> A s) ordered)
                 ; A "-o"
                 ; A ("as_ppx/ppx" ^ ext)]))
    ) [ ".byte",   ".cma",  ".cmo"
      ; ".native", ".cmxa", ".cmx" ]
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    js_hacks hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)

