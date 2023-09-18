open! Base
open Ppxlib
open Ppx_expect_runtime

val compact_loc_of_ppxlib_location : location -> Compact_loc.t

(** Matches an extension point payload that is either empty or a
    single string literal. The extracted information is the [string Payload.t]
    representing the string literal from the payload if present, or [{||}] if not. *)
val maybe_string_payload : unit -> (payload, string Payload.t -> 'a, 'a) Ast_pattern.t

val is_a_ppx_expect_ext_node : expression -> bool
