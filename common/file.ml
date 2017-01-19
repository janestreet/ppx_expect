open StdLabels
open MoreLabels
open Sexplib.Std
open Ppx_compare_lib.Builtin

module Name : sig
  type t [@@deriving sexp, compare]
  val to_string : t -> string
  val relative_to : dir:string -> t -> string
  val of_string : string -> t
end = struct
  type t = string [@@deriving sexp, compare]
  let to_string t = t
  let relative_to ~dir t =
    if not (Filename.is_relative t)
    then t
    else
      Filename.concat dir t
  let of_string s = s
end

let initial_dir =
  let dir_or_error =
    match Sys.getcwd () with
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
      { filename    : Name.t
      ; line_number : int
      ; line_start  : int
      ; start_pos   : int
      ; end_pos     : int
      }
    [@@deriving sexp, compare]

    let compare a b =
      if a.filename <> b.filename then
        invalid_arg "Expect_test_collector.File.Location.compare"
      else
        compare a b
    ;;
  end
  include T

  module Map = struct
    include Map.Make(T)

    let of_alist l =
      List.fold_left l ~init:empty ~f:(fun acc (loc, x) ->
        if mem loc acc then
          invalid_arg "Expect_test_collector.File.Location.Map.of_alist_exn";
        add ~key:loc ~data:x acc)
  end

  let beginning_of_file filename =
    { filename
    ; line_number = 1
    ; line_start  = 0
    ; start_pos   = 0
    ; end_pos     = 0
    }
  ;;
end

module Digest : sig
  type t [@@deriving sexp_of, compare]
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string [@@deriving sexp_of, compare]
  let to_string t = t
  let of_string s =
    let expected_length = 32 in
    if String.length s <> expected_length then
      invalid_arg "Expect_test_collector.File.Digest.of_string, unexpected length";
    for i = 0 to expected_length - 1 do
      match s.[i] with
      | '0' .. '9' | 'a' .. 'f' -> ()
      | _ -> invalid_arg "Expect_test_collector.File.Digest.of_string"
    done;
    s
end
