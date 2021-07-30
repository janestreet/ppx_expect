open! Base
open Import

module Name : sig
  type t [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t

    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val relative_to : dir:string -> t -> string

  include Identifiable.S with type t := t
end = struct
  include String

  let relative_to ~dir t =
    if not (Stdlib.Filename.is_relative t) then t else Stdlib.Filename.concat dir t
  ;;
end

let initial_dir =
  let dir_or_error =
    match Stdlib.Sys.getcwd () with
    | v -> `Ok v
    | exception exn -> `Exn exn
  in
  fun () ->
    match dir_or_error with
    | `Ok v -> v
    | `Exn exn -> raise exn
;;

module Location = struct
  module T = struct
    type t =
      { filename : Name.t
      ; line_number : int
      ; line_start : int
      ; start_pos : int
      ; end_pos : int
      }
    [@@deriving_inline sexp, compare]

    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let _tp_loc = "file.ml.Location.T.t" in
       function
       | Sexplib0.Sexp.List field_sexps as sexp ->
         let filename_field = ref Stdlib.Option.None
         and line_number_field = ref Stdlib.Option.None
         and line_start_field = ref Stdlib.Option.None
         and start_pos_field = ref Stdlib.Option.None
         and end_pos_field = ref Stdlib.Option.None
         and duplicates = ref []
         and extra = ref [] in
         let rec iter = function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
             :: tail ->
             let _field_sexp () =
               match _field_sexps with
               | [ x ] -> x
               | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
               | _ -> assert false
             in
             (match field_name with
              | "filename" ->
                (match !filename_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue = Name.t_of_sexp _field_sexp in
                   filename_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "line_number" ->
                (match !line_number_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue = int_of_sexp _field_sexp in
                   line_number_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "line_start" ->
                (match !line_start_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue = int_of_sexp _field_sexp in
                   line_start_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "start_pos" ->
                (match !start_pos_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue = int_of_sexp _field_sexp in
                   start_pos_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "end_pos" ->
                (match !end_pos_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue = int_of_sexp _field_sexp in
                   end_pos_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | _ ->
                if !Sexplib0.Sexp_conv.record_check_extra_fields
                then extra := field_name :: !extra
                else ());
             iter tail
           | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ()
         in
         iter field_sexps;
         (match !duplicates with
          | _ :: _ ->
            Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
          | [] ->
            (match !extra with
             | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
             | [] ->
               (match
                  ( !filename_field
                  , !line_number_field
                  , !line_start_field
                  , !start_pos_field
                  , !end_pos_field )
                with
                | ( Stdlib.Option.Some filename_value
                  , Stdlib.Option.Some line_number_value
                  , Stdlib.Option.Some line_start_value
                  , Stdlib.Option.Some start_pos_value
                  , Stdlib.Option.Some end_pos_value ) ->
                  { filename = filename_value
                  ; line_number = line_number_value
                  ; line_start = line_start_value
                  ; start_pos = start_pos_value
                  ; end_pos = end_pos_value
                  }
                | _ ->
                  Sexplib0.Sexp_conv_error.record_undefined_elements
                    _tp_loc
                    sexp
                    [ Sexplib0.Sexp_conv.( = ) !filename_field Stdlib.Option.None, "filename"
                    ; ( Sexplib0.Sexp_conv.( = ) !line_number_field Stdlib.Option.None
                      , "line_number" )
                    ; ( Sexplib0.Sexp_conv.( = ) !line_start_field Stdlib.Option.None
                      , "line_start" )
                    ; ( Sexplib0.Sexp_conv.( = ) !start_pos_field Stdlib.Option.None
                      , "start_pos" )
                    ; Sexplib0.Sexp_conv.( = ) !end_pos_field Stdlib.Option.None, "end_pos"
                    ])))
       | Sexplib0.Sexp.Atom _ as sexp ->
         Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
         : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { filename = v_filename
           ; line_number = v_line_number
           ; line_start = v_line_start
           ; start_pos = v_start_pos
           ; end_pos = v_end_pos
           } ->
        let bnds = [] in
        let bnds =
          let arg = sexp_of_int v_end_pos in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "end_pos"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_start_pos in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "start_pos"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_line_start in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_start"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_line_number in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_number"; arg ] :: bnds
        in
        let bnds =
          let arg = Name.sexp_of_t v_filename in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "filename"; arg ] :: bnds
        in
        Sexplib0.Sexp.List bnds
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    let compare =
      (fun a__001_ b__002_ ->
         if Ppx_compare_lib.phys_equal a__001_ b__002_
         then 0
         else (
           match Name.compare a__001_.filename b__002_.filename with
           | 0 ->
             (match compare_int a__001_.line_number b__002_.line_number with
              | 0 ->
                (match compare_int a__001_.line_start b__002_.line_start with
                 | 0 ->
                   (match compare_int a__001_.start_pos b__002_.start_pos with
                    | 0 -> compare_int a__001_.end_pos b__002_.end_pos
                    | n -> n)
                 | n -> n)
              | n -> n)
           | n -> n)
           : t -> t -> int)
    ;;

    let _ = compare

    [@@@end]

    let compare a b =
      if not (Name.equal a.filename b.filename)
      then invalid_arg "Expect_test_collector.File.Location.compare"
      else compare a b
    ;;
  end

  include T
  include Comparable.Make (T)

  let beginning_of_file filename =
    { filename; line_number = 1; line_start = 0; start_pos = 0; end_pos = 0 }
  ;;
end

module Digest : sig
  type t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val compare : t -> t -> int
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : t) -> ()
  let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t
  let compare = (compare_string : t -> t -> int)
  let _ = compare

  [@@@end]

  let to_string t = t

  let of_string s =
    let expected_length = 32 in
    if String.length s <> expected_length
    then invalid_arg "Expect_test_collector.File.Digest.of_string, unexpected length";
    for i = 0 to expected_length - 1 do
      match s.[i] with
      | '0' .. '9' | 'a' .. 'f' -> ()
      | _ -> invalid_arg "Expect_test_collector.File.Digest.of_string"
    done;
    s
  ;;
end
