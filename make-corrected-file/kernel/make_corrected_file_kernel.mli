(** Low-dependency version of [Make_corrected_file]. *)

val f
  :  ?use_dot_patdiff:bool
  -> ?corrected_path:string
  -> ?use_color:bool
  -> ?diff_command:string
  -> ?diff_path_prefix:string
  -> ?error_message:string
       (** Printed to stderr before the diff when changes are found. Ignored on success. *)
  -> next_contents:string
  -> path:string
  -> unit
  -> (unit, [ `Changes_found ]) Result.t
