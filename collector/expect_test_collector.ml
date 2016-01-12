open Sexplib.Std

module List = ListLabels

module File = struct
  module Name : sig
    type t [@@deriving sexp, compare]
    val to_string : t -> string
    val of_string : string -> t
  end = struct
    type t = string [@@deriving sexp, compare]
    let to_string t = t
    let of_string s = s
  end

  module Location = struct
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
end

module Expectation = struct
  type t =
    { expected  : string
    ; tag       : string option
    ; is_exact  : bool
    }
end

type t =
  { file_digest     : File.Digest.t
  ; location        : File.Location.t
  ; expectations    : (File.Location.t * Expectation.t) list
  ; saved_output    : (File.Location.t * string) list
  ; trailing_output : string
  ; default_indent  : int
  }

let tests_run : t list ref = ref []

module Instance : sig
  type t

  val save_output : t -> File.Location.t -> unit

  val exec :
    file_digest    : File.Digest.t ->
    location       : File.Location.t ->
    expectations   : (File.Location.t * Expectation.t) list ->
    default_indent : int ->
    f              : (t -> unit) ->
    unit
end = struct
  module Running = struct
    type t =
      { mutable saved : (File.Location.t * string) list
      ; stdout_backup : Unix.file_descr
      ; fd            : Unix.file_descr
      ; filename      : File.Name.t
      } [@@deriving fields]

    let create () =
      let stdout_backup = Unix.dup Unix.stdout in
      let filename = Filename.temp_file "expect-test" "stdout" in
      let fd = Unix.openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
      Unix.dup2 fd Unix.stdout;
      { stdout_backup
      ; fd
      ; filename = File.Name.of_string filename
      ; saved    = []
      }
    ;;

    let cleanup t =
      Unix.close t.fd;
      Unix.unlink (File.Name.to_string t.filename);
      Unix.dup2 t.stdout_backup Unix.stdout;
      Unix.close t.stdout_backup;
    ;;

    let get_output t =
      flush stdout;
      let output =
        let c = open_in (File.Name.to_string t.filename) in
        match really_input_string c (in_channel_length c) with
        | exception exn ->
          close_in c;
          raise exn
        | output ->
          close_in c;
          output
      in
      assert (Unix.lseek t.fd 0 SEEK_SET = 0);
      Unix.ftruncate t.fd 0;
      output
    ;;
  end

  type state = Running of Running.t | Ended
  type t = { mutable state : state }

  let exec ~file_digest ~location ~expectations ~default_indent ~f =
    let running = Running.create () in
    let t = { state = Running running } in
    let finally ~trailing_output =
      t.state <- Ended;
      Running.cleanup running;
      tests_run :=
        { file_digest
        ; location
        ; expectations
        ; saved_output = running.saved
        ; trailing_output
        ; default_indent
        } :: !tests_run;
    in
    match f t; Running.get_output running with
    | trailing_output -> finally ~trailing_output
    | exception exn ->
      finally ~trailing_output:"";
      raise exn

  ;;

  let save_output t location =
    match t.state with
    | Running running ->
      let output = Running.get_output running in
      running.saved <- (location, output) :: running.saved
    | Ended ->
      Printf.ksprintf failwith
        !"Expect_test_collector.Instance.save_output called after test has ended \
          (loc = %{sexp:File.Location.t})"
        location
  ;;
end

let run
      ~file_digest
      ~(location:File.Location.t)
      ~expectations
      ~default_indent
      f
  =
  Ppx_inline_test_lib.Runtime.test
    ""
    (File.Name.to_string location.filename)
    location.line_number
    (location.start_pos - location.line_start)
    (location.end_pos   - location.line_start)
    (fun () ->
       Instance.exec ~file_digest ~location ~expectations ~default_indent ~f;
       true
    );
;;

let tests_run () = !tests_run
