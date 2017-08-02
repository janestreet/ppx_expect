open Base
open Stdio
open Expect_test_common.Std
open Expect_test_matcher.Std

module Test_result = Ppx_inline_test_lib.Runtime.Test_result
module Collector_test_outcome = Expect_test_collector.Test_outcome

type group =
  { filename      : File.Name.t
  ; file_contents : string
  ; tests         : Matcher.Test_outcome.t Map.M(File.Location).t
  }

let convert_collector_test (test : Collector_test_outcome.t)
  : File.Location.t * Matcher.Test_outcome.t =
  let saved_output =
    Map.of_alist_multi (module File.Location) test.saved_output
    |> Map.map ~f:Matcher.Saved_output.of_nonempty_list_exn
  in
  let expectations =
    List.map test.expectations ~f:(fun (expect : Expectation.Raw.t) ->
      (expect.extid_location, Expectation.map_pretty expect ~f:Lexer.parse_pretty))
    |> Map.of_alist_exn (module File.Location)
  in
  (test.location,
   { expectations
   ; saved_output
   ; trailing_output = Matcher.Saved_output.of_nonempty_list_exn [test.trailing_output]
   ; upon_unreleasable_issue = test.upon_unreleasable_issue
   })
;;

let create_group (filename, tests) =
  let module D = File.Digest in
  let expected_digest =
    match
      List.map tests ~f:(fun (t : Collector_test_outcome.t) -> t.file_digest)
      |> List.dedup_and_sort ~compare:D.compare
    with
    | [digest] -> digest
    | [] -> assert false
    | digests ->
      Printf.ksprintf failwith
        !"Expect tests make inconsistent assumption about file \"%{File.Name}\" \
          %{sexp:D.t list}"
        filename digests
  in
  let file_contents =
    In_channel.read_all (File.Name.relative_to ~dir:(File.initial_dir ()) filename)
  in
  let current_digest =
    Caml.Digest.string file_contents |> Caml.Digest.to_hex |> D.of_string
  in
  if D.compare expected_digest current_digest <> 0 then
    Printf.ksprintf failwith
      !"File \"%{File.Name}\" changed, you need rebuild inline_test_runner \
        to be able to run expect tests \
        (expected digest: %{D}, current digest: %{D})"
      filename expected_digest current_digest;
  let tests =
    List.map tests ~f:convert_collector_test
    |> Map.of_alist_reduce (module File.Location) ~f:Matcher.Test_outcome.merge_exn
  in
  { filename; file_contents; tests }
;;

let convert_collector_tests tests : group list =
  List.map tests ~f:(fun (test : Collector_test_outcome.t) ->
    (test.location.filename, test))
  |> Map.of_alist_multi (module File.Name)
  |> Map.to_alist
  |> List.map ~f:create_group
;;

let process_group ~use_color ~in_place ~diff_command { filename; file_contents; tests }
  : Test_result.t =
  let bad_outcomes =
    Map.fold tests ~init:[] ~f:(fun ~key:location ~data:test acc ->
      match Matcher.evaluate_test ~file_contents ~location test with
      | Match -> acc
      | Correction c -> c :: acc)
    |> List.rev
  in
  let filename = File.Name.relative_to ~dir:(File.initial_dir ()) filename in
  let dot_corrected = filename ^ ".corrected" in
  let remove file =
    if Caml.Sys.file_exists file then Caml.Sys.remove file
  in
  match bad_outcomes with
  | [] ->
    remove dot_corrected;
    Success
  | _ ->
    (* We need a temporary file for corrections to allow [Print_diff] to work when
       multiple inline_tests_runner are run simultaneously. Otherwise one copy may remove
       the corrected file before the other can print the diff. *)
    let tmp_corrected =
      Caml.Filename.temp_file (Caml.Filename.basename filename) ".tmp_corrected"
        ~temp_dir:(Caml.Filename.dirname filename)
    in
    Matcher.write_corrected bad_outcomes
      ~file:(if in_place then filename else tmp_corrected)
      ~file_contents ~mode:Inline_expect_test;
    if in_place then begin
      remove dot_corrected;
      remove tmp_corrected;
      Success
    end else begin
      Print_diff.print ~file1:filename ~file2:tmp_corrected ~use_color ?diff_command ();
      Caml.Sys.rename tmp_corrected dot_corrected;
      Failure
    end
;;

let evaluate_tests ~use_color ~in_place ~diff_command =
  convert_collector_tests (Expect_test_collector.tests_run ())
  |> List.map ~f:(process_group ~use_color ~in_place ~diff_command)
  |> Test_result.combine_all
;;

let () =
  Ppx_inline_test_lib.Runtime.add_evaluator ~f:(fun () ->
    evaluate_tests
      ~use_color:Ppx_inline_test_lib.Runtime.use_color
      ~in_place:Ppx_inline_test_lib.Runtime.in_place
      ~diff_command:Ppx_inline_test_lib.Runtime.diff_command)
;;
