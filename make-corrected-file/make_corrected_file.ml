open! Base

let f
  ?use_dot_patdiff
  ?corrected_path
  ?use_color
  ?diff_command
  ?diff_path_prefix
  ~next_contents
  ~path
  ()
  =
  Make_corrected_file_kernel.f
    ?use_dot_patdiff
    ?corrected_path
    ?use_color
    ?diff_command
    ?diff_path_prefix
    ~next_contents
    ~path
    ()
  |> Result.map_error ~f:(fun `Changes_found -> Error.of_string "Changes found.")
;;
