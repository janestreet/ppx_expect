let rstrip ~drop string =
  let rec loop pos =
    if pos = 0 || not (drop (String.get string (pos - 1))) then pos else loop (pos - 1)
  in
  StringLabels.sub string ~pos:0 ~len:(loop (String.length string))
;;

let chop_prefix_if_exists string ~prefix =
  if StringLabels.starts_with string ~prefix
  then
    StringLabels.sub
      string
      ~pos:(String.length prefix)
      ~len:(String.length string - String.length prefix)
  else string
;;

let ensure_trailing_slash path = rstrip ~drop:(Char.equal '/') path ^ "/"

let chop_if_exists ~ancestor ~from:path =
  chop_prefix_if_exists path ~prefix:(ensure_trailing_slash ancestor)
;;

let f
  ?(use_dot_patdiff = false)
  ?corrected_path
  ?(use_color = false)
  ?diff_command
  ?diff_path_prefix
  ~next_contents
  ~path
  ()
  =
  let prev_contents =
    if Stdlib.Sys.file_exists path
    then In_channel.with_open_bin path In_channel.input_all
    else ""
  in
  match String.equal prev_contents next_contents with
  | true -> Ok ()
  | false ->
    let default_corrected_path = path ^ ".corrected" in
    let corrected_path = Option.value corrected_path ~default:default_corrected_path in
    Out_channel.with_open_bin corrected_path (fun oc ->
      Out_channel.output_string oc next_contents);
    let extra_patdiff_args =
      let default_configs =
        match use_dot_patdiff && Option.is_none (Sys.getenv_opt "TESTING_FRAMEWORK") with
        | true -> []
        | false -> [ "-default" ]
      in
      let cwd = Stdlib.Sys.getcwd () in
      let prefix =
        match diff_path_prefix with
        | Some prefix -> ensure_trailing_slash prefix
        | None -> ""
      in
      let alt_old = [ "-alt-old"; prefix ^ chop_if_exists ~ancestor:cwd ~from:path ] in
      let alt_new =
        [ "-alt-new"; prefix ^ chop_if_exists ~ancestor:cwd ~from:default_corrected_path ]
      in
      [ default_configs; alt_old; alt_new ] |> List.concat
    in
    Ppxlib_print_diff.print
      ?diff_command
      ~use_color
      ~extra_patdiff_args
      ~file1:path
      ~file2:corrected_path
      ();
    Error `Changes_found
;;
