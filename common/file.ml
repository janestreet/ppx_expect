open! Base
open Import

module Name : sig
  type t [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
    include Ppx_compare_lib.Comparable.S with type t := t
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
      (let error_source__002_ = "file.ml.Location.T.t" in
       fun x__003_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__002_
           ~fields:
             (Field
                { name = "filename"
                ; kind = Required
                ; conv = Name.t_of_sexp
                ; rest =
                    Field
                      { name = "line_number"
                      ; kind = Required
                      ; conv = int_of_sexp
                      ; rest =
                          Field
                            { name = "line_start"
                            ; kind = Required
                            ; conv = int_of_sexp
                            ; rest =
                                Field
                                  { name = "start_pos"
                                  ; kind = Required
                                  ; conv = int_of_sexp
                                  ; rest =
                                      Field
                                        { name = "end_pos"
                                        ; kind = Required
                                        ; conv = int_of_sexp
                                        ; rest = Empty
                                        }
                                  }
                            }
                      }
                })
           ~index_of_field:(function
             | "filename" -> 0
             | "line_number" -> 1
             | "line_start" -> 2
             | "start_pos" -> 3
             | "end_pos" -> 4
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:
             (fun
               (filename, (line_number, (line_start, (start_pos, (end_pos, ()))))) : t ->
               { filename; line_number; line_start; start_pos; end_pos })
           x__003_
         : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { filename = filename__005_
           ; line_number = line_number__007_
           ; line_start = line_start__009_
           ; start_pos = start_pos__011_
           ; end_pos = end_pos__013_
           } ->
        let bnds__004_ = ([] : _ Stdlib.List.t) in
        let bnds__004_ =
          let arg__014_ = sexp_of_int end_pos__013_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "end_pos"; arg__014_ ] :: bnds__004_
           : _ Stdlib.List.t)
        in
        let bnds__004_ =
          let arg__012_ = sexp_of_int start_pos__011_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "start_pos"; arg__012_ ] :: bnds__004_
           : _ Stdlib.List.t)
        in
        let bnds__004_ =
          let arg__010_ = sexp_of_int line_start__009_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_start"; arg__010_ ]
           :: bnds__004_
           : _ Stdlib.List.t)
        in
        let bnds__004_ =
          let arg__008_ = sexp_of_int line_number__007_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "line_number"; arg__008_ ]
           :: bnds__004_
           : _ Stdlib.List.t)
        in
        let bnds__004_ =
          let arg__006_ = Name.sexp_of_t filename__005_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "filename"; arg__006_ ] :: bnds__004_
           : _ Stdlib.List.t)
        in
        Sexplib0.Sexp.List bnds__004_
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    let compare =
      (fun a__015_ b__016_ ->
         if Stdlib.( == ) a__015_ b__016_
         then 0
         else (
           match Name.compare a__015_.filename b__016_.filename with
           | 0 ->
             (match compare_int a__015_.line_number b__016_.line_number with
              | 0 ->
                (match compare_int a__015_.line_start b__016_.line_start with
                 | 0 ->
                   (match compare_int a__015_.start_pos b__016_.start_pos with
                    | 0 -> compare_int a__015_.end_pos b__016_.end_pos
                    | n -> n)
                 | n -> n)
              | n -> n)
           | n -> n)
           : t -> t -> int)
    ;;

    let _ = compare

    [@@@end]
  end

  include T
  include Comparable.Make (T)

  let beginning_of_file filename =
    { filename; line_number = 1; line_start = 0; start_pos = 0; end_pos = 0 }
  ;;

  let of_source_code_position (pos : Source_code_position.t) =
    { filename = Name.of_string (Stdlib.Filename.basename pos.pos_fname)
    ; line_number = pos.pos_lnum
    ; line_start = pos.pos_bol
    ; start_pos = pos.pos_cnum
    ; end_pos = pos.pos_cnum
    }
  ;;
end

module Digest : sig
  type t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    include Ppx_compare_lib.Comparable.S with type t := t
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
