open! Base
include Expectation_intf.Definitions

let with_behavior
  { node_type
  ; position
  ; behavior = _
  ; payload_type
  ; on_incorrect_output
  ; inconsistent_outputs_message
  }
  behavior
  =
  { node_type
  ; position
  ; behavior
  ; payload_type
  ; on_incorrect_output
  ; inconsistent_outputs_message
  }
;;

let loc { position; _ } =
  match position with
  | Overwrite loc | Insert { loc; _ } -> loc
;;

let expect payload loc =
  { node_type = Extension
  ; position = Overwrite loc
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Pretty.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = "expect"
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_exact payload loc =
  { node_type = Extension
  ; position = Overwrite loc
  ; behavior =
      (let loc = Some loc in
       Expect
         { payload = Payload.Exact.of_located_payload ~loc payload
         ; on_unreachable = Replace_with_unreachable
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Exact)
  ; on_incorrect_output = "expect_exact"
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_unreachable loc =
  { node_type = Extension
  ; position = Overwrite loc
  ; behavior = Unreachable { reachability_of_corrected = Can_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = "expect"
  ; inconsistent_outputs_message = "test output"
  }
;;

let expect_uncaught_exn payload loc =
  { node_type = Attribute
  ; position = Overwrite loc
  ; behavior =
      Expect
        { payload = Payload.Pretty.of_located_payload ~loc:None payload
        ; on_unreachable = Delete
        ; reachability = Must_reach
        }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = "expect.uncaught_exn"
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;

let expect_trailing virtual_loc =
  { node_type = Extension
  ; position = Insert virtual_loc
  ; behavior =
      (let loc = Some virtual_loc.loc in
       Expect
         { payload = Payload.default (Payload.Pretty.Contents.of_located_string ~loc "")
         ; on_unreachable = Silent
         ; reachability = Can_reach
         })
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = "expect"
  ; inconsistent_outputs_message = "trailing output"
  }
;;

let expect_no_uncaught_exn virtual_loc =
  { node_type = Attribute
  ; position = Insert virtual_loc
  ; behavior = Unreachable { reachability_of_corrected = Must_reach }
  ; payload_type = (module Payload.Pretty)
  ; on_incorrect_output = "expect.uncaught_exn"
  ; inconsistent_outputs_message = "uncaught exception"
  }
;;
