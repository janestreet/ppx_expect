open! Base
open Ppxlib

let very_ghost =
  object
    inherit Ppxlib.Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end
;;

let () =
  Ppxlib.Driver.register_transformation
    ~extensions:
      [ Extension.declare
          "duplicate-for-ppx-expect-internal-testing.duplicate"
          Extension.Context.structure_item
          Ast_pattern.(pstr __)
          (fun ~loc ~path:_ str ->
            let mod_ = Ast_builder.Default.pmod_structure ~loc str in
            [%stri
              include struct
                include [%m mod_]
                include [%m mod_]
              end]
            |> very_ghost#structure_item)
      ]
    "duplicate-for-ppx-expect-internal-testing"
;;
