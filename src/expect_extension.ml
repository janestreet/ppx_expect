open Parsetree
open Ppx_core.Std
open Ast_pattern
open Extension

(* An expect declaration resembles [%%expect {tag|...|tag}]. We allow arbitrary tags so
  that users can escape their strings properly if need be. *)
let expect =
  Expert.declare "expect"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:false)

(* An expect extension without pretty formatting *)
let expect_exact =
  Expert.declare "expect_exact"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~is_exact:true)

let expectations = [ expect; expect_exact ]

let opt_name_and_expr expr =
  pstr ((
    pstr_value nonrecursive (
      value_binding
        ~pat:(alt_option (pstring __) ppat_any)
        ~expr ^:: nil)
  ) ^:: nil)

let expect_test =
  Expert.declare "expect_test" Context.structure_item
    (opt_name_and_expr __)
    (fun name e -> (name, e))

let match_expectation e =
  match e.pexp_desc with
  | Pexp_extension extension -> begin
      match Expert.convert expectations ~loc:e.pexp_loc extension with
      | None -> None
      | Some f -> Some (f ~extension_id_loc:(fst extension).loc)
    end
  | _ -> None
;;

let match_expect_test st =
  match st.pstr_desc with
  | Pstr_extension (extension, attributes) -> begin
      match Expert.convert [expect_test] ~loc:st.pstr_loc extension with
      | None -> None
      | Some x -> begin
          assert_no_attributes attributes;
          Some x
        end
    end
  | _ -> None
;;

