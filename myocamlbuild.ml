(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

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
                      ppx_inline_test.libname,ppx_here.expander,sexplib"
                 ; S (List.map (fun s -> A s) ordered)
                 ; A "-o"
                 ; A ("as_ppx/ppx" ^ ext)]))
    ) [ ".byte",   ".cma",  ".cmo"
      ; ".native", ".cmxa", ".cmx" ]
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)

