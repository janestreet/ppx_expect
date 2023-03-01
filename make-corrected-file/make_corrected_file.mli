open! Base


(** [f ~contents ~path] compares the contents of [path] against [contents]. If they are
    not equal, it writes [contents] to [corrected_path], emits a build error, and
    returns [Error _].

    Regardless of [corrected_path], the diff is shown in terms of [path ^ ".corrected"].

    The caller should exit nonzero to indicate that a build error occurred. *)
val f
  :  ?use_dot_patdiff:bool (** default: [false] *)
  -> ?corrected_path:string (** default: [path ^ ".corrected"] *)
  -> ?use_color:bool (** default: [false] *)
  -> ?diff_command:string
  -> message:string option
  -> next_contents:string
  -> path:string
  -> unit
  -> unit Or_error.t
