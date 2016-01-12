open Parsetree
open Ppx_core.Std
open Ast_pattern
open Extension

type t =
  { expected : string
  ; tag      : string option
  ; is_exact : bool
  }

(* An expect declaration resembles [%%expect {tag|...|tag}]. We allow arbitrary tags so
  that users can escape their strings properly if need be. *)
let expect =
  Expert.declare "expect"
    Context.expression
    (single_expr_payload (pexp_constant (const_string __ __)))
    (fun expected tag -> { expected; tag; is_exact = false })

(* An expect extension without pretty formatting *)
let expect_exact =
  Expert.declare "expect_exact"
    Context.expression
    (single_expr_payload (pexp_constant (const_string __ __)))
    (fun expected tag -> { expected; tag; is_exact = true })

let expectations = [ expect; expect_exact ]

let opt_name_and_expr expr =
  pstr ((
    pstr_value nonrecursive (
      value_binding
        ~pat:(map (pstring __) ~f:(fun f x -> f (Some x)))
        ~expr ^:: nil)
    ||| map (pstr_eval expr nil) ~f:(fun f -> f None)
  ) ^:: nil)

let expect_test =
  Expert.declare "expect_test" Context.structure_item
    (opt_name_and_expr __)
    (fun name e -> (name, e))

let match_expectation e =
  match e.pexp_desc with
  | Pexp_extension extension ->
    Expert.convert expectations ~loc:e.pexp_loc extension
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

