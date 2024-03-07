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

  module Flexibility = struct
    type nonrec t =
      | Flexible_modulo of t
      | Exactly_formatted
  end
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

module String_node_format = struct
  type longhand = Longhand
  type shorthand = Shorthand

  module Hand = struct
    type _ t =
      | Longhand : longhand t
      | Shorthand : shorthand t
  end

  module Kind = struct
    type _ t =
      | Attribute : longhand t
      | Extension : _ t
  end

  module Shape = struct
    type 'hand unpacked =
      { name : string
      ; hand : 'hand Hand.t
      ; kind : 'hand Kind.t
      }

    type t = T : _ unpacked -> t [@@unboxed]
  end

  module Delimiter = struct
    type _ unpacked =
      | Quote : longhand unpacked
      | Tag : string -> _ unpacked

    type t = T : _ unpacked -> t [@@unboxed]

    let default = T (Tag "")

    let longhand = function
      | T ((Quote | Tag _) as unpacked) -> unpacked
    ;;

    let shorthand = function
      | T (Tag _ as unpacked) -> unpacked
      | T Quote -> Tag ""
    ;;

    let handed : type a. t -> a Hand.t -> a unpacked =
      fun t hand ->
      match hand with
      | Longhand -> longhand t
      | Shorthand -> shorthand t
    ;;
  end

  type 'a unpacked =
    { shape : 'a Shape.unpacked
    ; delimiter : 'a Delimiter.unpacked
    }

  type t = T : _ unpacked -> t [@@unboxed]
end
