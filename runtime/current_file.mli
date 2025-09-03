open! Base

(** We dynamically keep track of the "currently executing" file to make sure tests are not
    being run from outside the file in which they are defined. *)

val set : filename_rel_to_project_root:string -> unit
val unset : unit -> unit
val get : unit -> string

(** The absolute path of the working directory when the executable was first run.

    Forcing the [Lazy.t] raises if the initial call to [Stdlib.Sys.getcwd] raised. *)
val initial_dir : string Lazy.t

(** Given a path, removes the first element of that path.

    i.e. if given foo/bar/file, returns bar/file
   *)
val remove_top_dir: string -> string

(** Given a path relative to the initial working directory, returns an absolute path.

    Raises if the initial call to [Stdlib.Sys.getcwd] raised. *)
val absolute_path : string -> string

(** Verifies that [filename_rel_to_project_root] was the last file set as [current].

    Raises if not. *)
val verify_that_file_is_current_exn
  :  line_number:int
  -> filename_rel_to_project_root:string
  -> unit
