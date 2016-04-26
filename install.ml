#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_expect"
  [ oasis_lib "expect_test_collector"
  ; oasis_lib "expect_test_common"
  ; oasis_lib "expect_test_config"
  ; oasis_lib "expect_test_matcher"
  ; oasis_lib "ppx_expect"
  ; oasis_obj "ppx_expect_evaluator"
  ; oasis_lib "ppx_expect_payload"
  ; file "META" ~section:"lib"
  ; oasis_exe "ppx" ~dest:"../lib/ppx_expect/ppx"
  ; file "_build/namespace_wrappers/ocaml_re.cmi" ~section:"lib"
  ]
