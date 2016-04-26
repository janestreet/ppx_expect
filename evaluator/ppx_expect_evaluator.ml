open Expect_test_common.Std
open Expect_test_matcher.Std
open StdLabels
open MoreLabels
open Sexplib.Std

module Test_result = Ppx_inline_test_lib.Runtime.Test_result
module Collector_test_outcome = Expect_test_collector.Test_outcome

type group =
  { filename      : File.Name.t
  ; file_contents : string
  ; tests         : Matcher.Test_outcome.t File.Location.Map.t
  }

let convert_collector_test ~filename (test : Collector_test_outcome.t)
  : File.Location.t * Matcher.Test_outcome.t =
  let saved_output =
    List.fold_left test.saved_output ~init:File.Location.Map.empty ~f:(fun acc (loc, x) ->
      if File.Location.Map.mem loc acc then
        Printf.ksprintf failwith !"Output collector at %{File.Name}:%d ran more than once"
          filename loc.line_number ()
      else
        File.Location.Map.add ~key:loc ~data:x acc)
  in
  let expectations =
    List.map test.expectations ~f:(fun (expect : Expectation.Raw.t) ->
      (expect.extid_location,
       Expectation.map_pretty expect ~f:Lexer.parse_pretty)
    )
    |> File.Location.Map.of_alist
  in
  (test.location,
   { expectations
   ; saved_output
   ; trailing_output = test.trailing_output
   })
;;

let create_group (filename, tests) =
  let module D = File.Digest in
  let expected_digest =
    let module DSet = Set.Make(D) in
    match
      List.fold_left tests ~init:DSet.empty ~f:(fun acc (t : Collector_test_outcome.t) ->
        DSet.add t.file_digest acc)
      |> DSet.elements
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
    let ic = open_in (File.Name.to_string filename) in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; s
    | exception e -> close_in ic; raise e
  in
  let current_digest = Digest.string file_contents |> Digest.to_hex |> D.of_string in
  if D.compare expected_digest current_digest <> 0 then
    Printf.ksprintf failwith
      !"File \"%{File.Name}\" changed, you need rebuild inline_test_runner \
        to be able to run expect tests \
        (expected digest: %{D}, current digest: %{D})"
      filename expected_digest current_digest;
  let tests =
    List.map tests ~f:(convert_collector_test ~filename)
    |> File.Location.Map.of_alist
  in
  { filename
  ; file_contents
  ; tests = tests
  }
;;

let convert_collector_tests tests : group list =
  let module M = Map.Make(File.Name) in
  List.fold_left tests ~init:M.empty
    ~f:(fun acc (test : Collector_test_outcome.t) ->
      let key = test.location.filename in
      let l =
        match M.find key acc with
        | l -> l
        | exception Not_found -> []
      in
      M.add ~key ~data:(test :: l) acc)
  |> M.bindings
  |> List.map ~f:create_group
;;

let process_group ~use_color { filename; file_contents; tests } =
  let target = File.Name.to_string filename ^ ".corrected" in
  let bad_outcomes =
    File.Location.Map.fold tests ~init:[] ~f:(fun ~key:location ~data:test acc ->
      match Matcher.evaluate_test ~file_contents ~location test with
      | Match -> acc
      | Correction c -> c :: acc)
    |> List.rev
  in
  match bad_outcomes with
  | [] ->
    if Sys.file_exists target then Sys.remove target;
    Test_result.Success
  | _ ->
    Matcher.write_corrected bad_outcomes
      ~file:target ~file_contents ~mode:Inline_expect_test;
    Matcher.print_diff ?diff_command:Ppx_inline_test_lib.Runtime.diff_command
      ~file1:(File.Name.to_string filename) ~file2:target ~use_color ();
    Test_result.Failure
;;

let evaluate_tests ~use_color =
  convert_collector_tests (Expect_test_collector.tests_run ())
  |> List.map ~f:(process_group ~use_color)
  |> Test_result.combine_all
;;

let () =
  Ppx_inline_test_lib.Runtime.add_evaluator ~f:(fun () ->
    evaluate_tests ~use_color:Ppx_inline_test_lib.Runtime.use_color)
;;
