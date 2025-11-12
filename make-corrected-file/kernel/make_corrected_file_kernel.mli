(** Low-dependency version of [Make_corrected_file]. *)

val f
  :  ?use_dot_patdiff:bool
  -> ?corrected_path:string
  -> ?use_color:bool
  -> ?diff_command:string
  -> ?diff_path_prefix:string
  -> next_contents:string
  -> path:string
  -> unit
  -> (unit, [ `Changes_found ]) Result.t
