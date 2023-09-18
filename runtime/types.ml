open! Base

module Compact_loc = struct
  type t =
    { start_bol : int
    ; start_pos : int
    ; end_pos : int
    }

  let equal a b =
    a.start_bol = b.start_bol && a.start_pos = b.start_pos && a.end_pos = b.end_pos
  ;;

  let compare_character_range =
    Comparable.lexicographic
      [ Comparable.lift compare_int ~f:(fun t -> t.start_pos)
      ; Comparable.lift compare_int ~f:(fun t -> t.end_pos)
      ]
  ;;
end

module Expect_node_formatting = struct
  type t =
    { indent : int
    ; always_on_own_line : bool
    ; extension_sigil : string
    ; attribute_sigil : string
    }

  let default =
    { indent = 2
    ; always_on_own_line = false
    ; extension_sigil = "%"
    ; attribute_sigil = "@@"
    }
  ;;
end

module Virtual_loc = struct
  type t =
    { loc : Compact_loc.t
    ; body_loc : Compact_loc.t
    }
end

module Expectation_id = struct
  include Int

  let mint =
    let counter = ref 0 in
    fun () ->
      let id = !counter in
      counter := id + 1;
      id
  ;;
end
