open! Base
open Types

module Patch_with_file_contents = struct
  type 'a t = original_file_contents:string -> 'a -> (Compact_loc.t * string) list
end

let rewrite_corrections ~original_file_contents ~corrections =
  (* Ensure that we encounter the corrections in order as we build up the file. *)
  let corrections =
    List.sort
      ~compare:(Comparable.lift Compact_loc.compare_character_range ~f:fst)
      corrections
  in
  let l_pos, strs =
    List.fold_map
      corrections
      ~init:0
      ~f:(fun l_pos ({ start_pos; end_pos; start_bol = _ }, correction) ->
      let code_chunk =
        String.sub original_file_contents ~pos:l_pos ~len:(start_pos - l_pos)
      in
      end_pos, [ code_chunk; correction ])
  in
  let result = List.concat strs |> String.concat in
  let rest = String.subo original_file_contents ~pos:l_pos in
  result ^ rest
;;

let f ~use_color ~in_place ~diff_command ~diff_path_prefix ~filename ~with_ corrections
  : Ppx_inline_test_lib.Test_result.t
  =
  let dot_corrected = filename ^ ".corrected" in
  let original_file_contents =
    let in_channel = Stdlib.open_in_bin filename in
    let contents =
      Stdlib.really_input_string in_channel (Stdlib.in_channel_length in_channel)
    in
    Stdlib.close_in in_channel;
    contents
  in
  let remove file = if Stdlib.Sys.file_exists file then Stdlib.Sys.remove file in
  let corrections = with_ ~original_file_contents corrections in
  let next_contents = rewrite_corrections ~original_file_contents ~corrections in
  match in_place with
  | true ->
    if not (String.equal original_file_contents next_contents)
    then Stdio.Out_channel.write_all filename ~data:next_contents;
    remove dot_corrected;
    Success
  | false ->
    (match diff_command with
     | Some "-" (* Just write the .corrected file - do not output a diff. *) ->
       Stdio.Out_channel.write_all dot_corrected ~data:next_contents;
       Success
     | _ ->
       (* By invoking [Make_corrected_file.f] with a fresh temporary file, we avoid the
          following possible race between inline_test_runners A and B:
          1. A runs test T1 and generates next contents C1.
          2. B runs test T2 and generates next contents C2.
          3. A writes C1 to the .corrected file.
          4. B writes C2 to the .corrected file.
          5. A diffs the .corrected file against the original file and reports the
          result. It thinks it is reporting the diff produced by T1, but is in fact
          reporting the diff produced by T2. The key aspect of using temporary files is
          that even if in the above scenario the final contents of the .corrected file
          are C2, the diff reported by A comes from its tmp file and will still be the
          diff produced by T1. *)
       let tmp_corrected =
         Stdlib.Filename.temp_file
           (Stdlib.Filename.basename filename)
           ".corrected.tmp"
           ~temp_dir:(Stdlib.Filename.dirname filename)
       in
       (match
          Make_corrected_file.f
            ~use_color
            ?diff_command
            ?diff_path_prefix
            ~corrected_path:tmp_corrected
            ~next_contents
            ~path:filename
            ()
        with
        | Ok _ ->
          (* Even though this execution of the expect test ran without making
             corrections, we should delete any old [.corrected] files that are left over
             from previous builds. In particular, hydra relies on this behavior for
             flaky tests; if the test fails the first time and passes the second, the
             second run should make sure the [.corrected] file is not lingering in the
             sandbox. *)
          remove dot_corrected;
          remove tmp_corrected;
          Success
        | Error _ ->
          Stdlib.Sys.rename tmp_corrected dot_corrected;
          Failure))
;;
