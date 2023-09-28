open! Base
open Types
include Expectation_intf.Definitions

let with_behavior
  { position
  ; behavior = _
  ; payload_type
  ; on_incorrect_output
  ; inconsistent_outputs_message
  }
  behavior
  =
  { position; behavior; payload_type; on_incorrect_output; inconsistent_outputs_message }
;;

let loc { position; _ } =
  match position with
  | Overwrite { whole_node = loc; payload = _ } | Insert { loc; _ } -> loc
;;

let extension_syntax extension_name ~payload_loc ~loc =
  let contains (outer : Compact_loc.t) ~(inner : Compact_loc.t) =
    outer.start_pos <= inner.start_pos && outer.end_pos >= inner.end_pos
  in
  match payload_loc with
  | Some payload_loc when contains payload_loc ~inner:loc ->
    (* An extension point whose payload location contains the location of the entire
       extension point is using the "shorthand" syntax. *)
    (T { name = extension_name; kind = Extension; hand = Shorthand }
      : String_node_format.Shape.t)
  | _ -> T { name = extension_name; kind = Extension; hand = Longhand }
;;

let expect ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Pretty.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = extension_syntax "expect" ~payload_loc ~loc
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_exact ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Exact.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Exact)
  ; on_incorrect_output = extension_syntax "expect_exact" ~payload_loc ~loc
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_unreachable loc =
  { position = Overwrite { whole_node = loc; payload = None }
  ; behavior = Unreachable { reachability_of_corrected = Can_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_uncaught_exn ~payload_loc payload loc =
  { position = Overwrite { whole_node = loc; payload = payload_loc }
  ; behavior =
      Expect
        { payload = Payload.Pretty.of_located_payload ~loc:None payload
        ; on_unreachable = Delete
        ; reachability = Must_reach
        }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;

let expect_trailing virtual_loc =
  { position = Insert virtual_loc
  ; behavior =
      (let loc = Some virtual_loc.loc in
       Expect
         { payload = Payload.default (Payload.Pretty.Contents.of_located_string ~loc "")
         ; on_unreachable = Silent
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = T { name = "expect"; kind = Extension; hand = Longhand }
  ; inconsistent_outputs_message = "trailing output"
  }
;;

let expect_no_uncaught_exn virtual_loc =
  { position = Insert virtual_loc
  ; behavior = Unreachable { reachability_of_corrected = Must_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output =
      T { name = "expect.uncaught_exn"; kind = Attribute; hand = Longhand }
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;
