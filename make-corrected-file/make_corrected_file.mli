open! Base

(** [f ~next_contents ~path ()] compares the contents of [path] against [next_contents].
    If the contents are unchanged, [f] returns [Ok ()]. If they are changed, it writes
    [next_contents] to [corrected_path], emits a build error, and returns [Error _]. The
    caller should exit nonzero (possibly by raising the returned error) to indicate to the
    build that an error occurred. If it doesn't, the build system may not recognize that a
    corrected file has been generated and needs to be moved out of a sandbox.

    The optional arguments support "expert" use cases. Most clients do not need them. *)
val f
  :  ?use_dot_patdiff:bool (** default: [false] *)
  -> ?corrected_path:string (** default: [path ^ ".corrected"] *)
  -> ?use_color:bool (** default: [false] *)
  -> ?diff_command:string
  -> ?diff_path_prefix:string
  -> next_contents:string
  -> path:string
  -> unit
  -> unit Or_error.t
