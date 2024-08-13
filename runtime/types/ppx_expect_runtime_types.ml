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

  type location_type =
    | Parsed
    | Trailing
    | Exception

  let lookup_or_mint =
    let counter = ref 0 in
    let location_map =
      Hashtbl.create
        (module struct
          type t = location_type * Compact_loc.t

          let loc_type_to_int = function
            | Parsed -> 0
            | Trailing -> 1
            | Exception -> 2
          ;;

          let compare =
            Comparable.lexicographic
              [ Comparable.lift compare_int ~f:(fun (loc_type, _) ->
                  loc_type_to_int loc_type)
              ; Comparable.lift Compact_loc.compare_character_range ~f:snd
              ]
          ;;

          let sexp_of_t _ = Sexp.Atom "opaque"

          let hash (loc_type, ({ start_bol; start_pos; end_pos } : Compact_loc.t)) =
            let hash_fold_int = Fn.flip hash_fold_int in
            Hash.create ()
            |> hash_fold_int (loc_type_to_int loc_type)
            |> hash_fold_int start_bol
            |> hash_fold_int start_pos
            |> hash_fold_int end_pos
            |> Hash.get_hash_value
          ;;
        end)
    in
    fun loc_type loc ->
      Hashtbl.find_or_add location_map (loc_type, loc) ~default:(fun () ->
        let id = !counter in
        counter := id + 1;
        id)
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

module Payload = struct
  type t =
    { contents : string
    ; tag : String_node_format.Delimiter.t
    }

  let default contents = { contents; tag = String_node_format.Delimiter.default }

  let to_source_code_string { contents; tag } =
    let escape_lines test_output =
      test_output
      |> String.split ~on:'\n'
      |> List.map ~f:String.escaped
      |> String.concat ~sep:"\n"
    in
    match tag with
    | T (Tag tag) -> Printf.sprintf "{%s|%s|%s}" tag contents tag
    | T Quote -> Printf.sprintf {|"%s"|} (escape_lines contents)
  ;;
end
