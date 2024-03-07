open! Base

module Expect_node_formatting : sig
  (** Configurations for the formatting of rewritten expect nodes and attributes. The
      values in [default] are used by [ppx_expect], but different values can be used by
      other clients of the expect test runtime. *)
  type t =
    { indent : int
        (** The number of spaces that the bodies of [[%expect]] nodes are indented with
        respect to the left edge of the extension point and that trailing [[%expect]]
        nodes are indented with respect to the enclosing [let%expect] *)
    ; always_on_own_line : bool
        (** Whether the output of [[%expect]] nodes should always be formatted so it gets its
        own lines, even if it is only one line long. If this option is true, one-line
        expectations will be printed like

        {v
        [%expect {|
          foo
        |}]
        v}

        rather than like

        {v
        [%expect {| foo |}]
        v}
    *)
    ; extension_sigil : string
        (** The sigil that should be printed to signal the start of an extension point. By
        default, this is ["%"], though in toplevel tests it changes to ["%%"]. *)
    ; attribute_sigil : string
        (** The sigil that should be printed to signal the start of an attribute. By default,
        this is ["@@"]. *)
    }

  (** The default formatting configuration used in expect tests. *)
  val default : t

  module Flexibility : sig
    type expect_node_formatting := t

    type t =
      | Flexible_modulo of expect_node_formatting
      | Exactly_formatted
  end
end

module Compact_loc : sig
  (** A range of characters in a source file.

      Notably, [Compact_loc.t] does not store the name of the file itself; it only
      represents the range of characters.

      Consider this diagram, where the characters [/[a-b]/] represent the included range,
      and the characters [/[.0]/] represent the surrounding text:

      {v
      .......
      0.........a-----
      ------------
      -----b
      .........
      v}

      Then [start_bol] is the position of [0], [start_pos] of [a], and [end_pos] of [b].

      This record corresponds to a [Ppxlib.Location.t] restricted to just the
      [loc_start.pos_cbol], [loc_start.pos_cnum], and [loc_end.pos_cnum] fields.
  *)
  type t =
    { start_bol : int (** Index of the first character of the first line in the range. *)
    ; start_pos : int (** Index of the first character in the range. *)
    ; end_pos : int (** Index of the last character in the range. *)
    }

  (** [Compact_loc.t] satisfies the natural definition of equality. We write it by hand
      only to minimize external dependencies. *)
  val equal : t -> t -> bool

  (** Compares the range of characters spanned by a [Compact_loc.t]. Ranges that start
      earlier are considered smaller. For ranges that start at the same position, ranges
      that end earlier are considered smaller. *)
  val compare_character_range : t -> t -> int
end

module Virtual_loc : sig
  (** Information that should be taken into account when inserting and formatting
      "virtual" test nodes that do not appear in the original file, like
      [[@@expect.uncaught_exn]] nodes and [[%expect]] nodes with the trailing output of a
      test. *)
  type t =
    { loc : Compact_loc.t
        (** The location where the expect node should be inserted on failure *)
    ; body_loc : Compact_loc.t
        (** The location spanning from the beginning of the [let%expect_test] binding to the
        end of the expression in the body of the test. *)
    }
end

module Expectation_id : sig
  (** An identifier for a test node. Each test node that was parsed from an extension
      point or attribute ([[%expect]], [[%expect_exact]], [[%expect.unreachable]],
      or [[%expect.uncaught_exn]]) or that might be added into the corrected file (e.g. a
      [[%expect]] for trailing output) is associated with a unique [t]. *)
  type t

  include Intable.S with type t := t
  include Hashable.Key with type t := t

  (** Create a new [t]. Calls to [mint] will give distinct ids, but uniqueness is not
      guaranteed if [of_int_exn] is used to create an id. *)
  val mint : unit -> t
end

module String_node_format : sig
  (** The syntactic format of attributes and extension points carrying string payloads. *)

  (** Phantom type for longhand syntax: [[%foo {||}]] or [[@foo {||}]]. *)
  type longhand = Longhand

  (** Phantom type for shorthand syntax: [{%foo||}]. *)
  type shorthand = Shorthand

  module Hand : sig
    (** "Handedness" of syntax: longhand or shorthand, as described above. **)
    type _ t =
      | Longhand : longhand t
      | Shorthand : shorthand t
  end

  module Kind : sig
    (** Kind of node: attribute or extension. There is no shorthand for attributes with
        string payloads, so attributes can only be longhand here. *)
    type _ t =
      | Attribute : longhand t
      | Extension : _ t
  end

  module Shape : sig
    (** Shape of a string node: its name (e.g. "expect"), handedness, and node kind. This
        is preserved when rewriting the node. *)
    type 'hand unpacked =
      { name : string
      ; hand : 'hand Hand.t
      ; kind : 'hand Kind.t
      }

    type t = T : _ unpacked -> t [@@unboxed]
  end

  module Delimiter : sig
    (** Delimiter around string constant. *)
    type _ unpacked =
      | Quote : longhand unpacked (** Quoted strings, e.g. ["foo"]. *)
      | Tag : string -> _ unpacked (** Tagged strings, e.g. [{tag|foo|tag}]. *)

    type t = T : _ unpacked -> t [@@unboxed]

    (** Default delimiter: [T (Tag "")]. *)
    val default : t

    (** If given [Quoted] and [Shorthand], produces [Tag ""]. In any other case, produces
        the given delimiter. *)
    val handed : t -> 'a Hand.t -> 'a unpacked
  end

  (** Format of a string node: its shape, and a compatible delimiter. We preserve its
      shape across rewrites, but may have to change its delimiter. For example, if
      [{bar|foo|bar}] needs to match ["bar"], we must replace its tag. *)
  type 'a unpacked =
    { shape : 'a Shape.unpacked
    ; delimiter : 'a Delimiter.unpacked
    }

  type t = T : _ unpacked -> t [@@unboxed]
end
