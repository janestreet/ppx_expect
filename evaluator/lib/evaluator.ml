open StdLabels
open MoreLabels
open Sexplib.Std

module Test_result = Ppx_inline_test_lib.Runtime.Test_result

module File = struct
  open Expect_test_collector

  module Digest = File.Digest

  module Name = File.Name

  module Location = struct
    include File.Location

    module Map = struct
      include Map.Make(File.Location)

      let of_alist_exn l =
        List.fold_left l ~init:empty ~f:(fun acc (loc, x) ->
          assert (not (mem loc acc));
          add ~key:loc ~data:x acc)
    end

    let beginning_of_file filename =
      { filename
      ; line_number = 1
      ; line_start  = 0
      ; start_pos   = 0
      ; end_pos     = 0
      }
    ;;
  end
end

type test =
  { expectations    : Expectation.t File.Location.Map.t
  ; saved_output    : string File.Location.Map.t
  ; trailing_output : string
  ; default_indent  : int
  }

type group =
  { filename      : File.Name.t
  ; file_contents : string
  ; tests         : test File.Location.Map.t
  }

let output_slice out s a b =
  output_string out (String.sub s ~pos:a ~len:(b - a))
;;

let evaluate_test t file_contents (location : File.Location.t) ~out =
  let (test_results, last_location) =
    File.Location.Map.fold t.expectations
      ~init:([], { location with end_pos = location.start_pos })
      ~f:(fun ~key:location ~data (test_results, (last_location : File.Location.t)) ->
        output_slice out file_contents last_location.end_pos location.start_pos;
        let test_result =
          match File.Location.Map.find location t.saved_output with
          | exception Not_found -> begin
              output_string out "[%collector_never_triggered]";
              Test_result.Failure
            end
          | actual -> begin
              let expect: Expectation.t = data in
              let default_indent = location.start_pos - location.line_start in
              let (expect, test_result) =
                match Reconcile.expectation ~expect ~actual ~default_indent with
                | Match -> (expect, Test_result.Success)
                | Correction expect -> (expect, Test_result.Failure)
              in
              output_string out expect.as_string;
              test_result
            end
        in
        (test_result :: test_results, location)
      )
  in

  output_slice out file_contents last_location.end_pos location.end_pos;

  let trailing_output_result =
    let empty_expect: Expectation.t = {
      as_string = "";
      tag = Some "";
      body = Pretty Expectation.Pretty.empty;
    }
    in
    match
      Reconcile.expectation
        ~expect:empty_expect
        ~actual:t.trailing_output
        ~default_indent:t.default_indent
    with
    | Reconcile.Match -> Test_result.Success
    | Reconcile.Correction expect -> begin
        let rec last_non_whitespace_char pos =
          if pos < 0 then
            None
          else
            match file_contents.[pos] with
            | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' ->
              last_non_whitespace_char (pos - 1)
            | c -> Some c
        in
        begin match last_non_whitespace_char location.end_pos with
        | None | Some ';' -> ()
        | _ -> output_char out ';'
        end;
        let default_indent = String.make t.default_indent ' ' in
        Printf.fprintf out "\n%s%s" default_indent expect.as_string;
        Test_result.Failure
      end
  in

  Test_result.combine_all (trailing_output_result :: test_results)
;;

let patdiff_cmd ~use_color =
  let args =
    List.concat [
      ["-keep-whitespace"];
      ["-location-style omake"];
      (if use_color then [] else ["-ascii"]);
    ]
  in
  String.concat ~sep:" " ("patdiff" :: args)
;;

let print_diff ~file1 ~file2 ~use_color =
  let exec cmd =
    let cmd =
      Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1) (Filename.quote file2)
    in
    match Sys.command cmd with
    | 0 -> true
    | 1 -> false
    | n -> Printf.eprintf "%S exited with code %d\n" cmd n; exit 2
  in
  if exec (patdiff_cmd ~use_color) then
    ignore (exec "diff -u" : bool)
;;

let convert_collector_test ~filename ~file_contents (test : Expect_test_collector.t) =
  let { Expect_test_collector.
        location
      ; expectations
      ; saved_output
      ; trailing_output
      ; default_indent
      ; _
      } = test
  in
  let saved_output =
    List.fold_left saved_output ~init:File.Location.Map.empty ~f:(fun acc (loc, x) ->
      if File.Location.Map.mem loc acc then
        Printf.ksprintf failwith !"Output collector at %{File.Name}:%d ran more than once"
          filename loc.line_number ()
      else
        File.Location.Map.add ~key:loc ~data:x acc)
  in
  let expectations =
    List.map expectations ~f:(fun (loc, expect) ->
      let { File.Location. start_pos; end_pos; _ } = loc in
      let { Expect_test_collector.Expectation. expected; tag; is_exact } = expect in
      let as_string =
        String.sub file_contents ~pos:start_pos ~len:(end_pos - start_pos)
      in
      (loc,
       { Expectation.
         as_string
       ; tag
       ; body =
           Lexer.parse_body expected
             ~kind:(if is_exact then Exact else Pretty)
       })
    )
    |> File.Location.Map.of_alist_exn
  in
  (location,
   { expectations
   ; saved_output
   ; trailing_output
   ; default_indent
   })
;;

let create_group (filename, tests) =
  let module D = File.Digest in
  let expected_digest =
    let module DSet = Set.Make(D) in
    match
      List.fold_left tests ~init:DSet.empty ~f:(fun acc (t : Expect_test_collector.t) ->
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
    List.map tests ~f:(convert_collector_test ~filename ~file_contents)
    |> File.Location.Map.of_alist_exn
  in
  { filename
  ; file_contents
  ; tests = tests
  }
;;

let convert_collector_tests tests : group list =
  let module M = Map.Make(File.Name) in
  List.fold_left tests ~init:M.empty
    ~f:(fun acc (test : Expect_test_collector.t) ->
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
  let file_length = String.length file_contents in
  let target = File.Name.to_string filename ^ ".corrected" in
  let test_result =
    let out = open_out target in
    match
      let (test_results, last_location) =
        File.Location.Map.fold tests
          ~init:([], File.Location.beginning_of_file filename)
          ~f:(fun ~key:location ~data (test_results, (last_location : File.Location.t)) ->
            output_slice out file_contents last_location.end_pos location.start_pos;
            (evaluate_test ~out data file_contents location :: test_results, location)
          )
      in
      output_slice out file_contents last_location.end_pos file_length;
      Test_result.combine_all test_results
    with
    | x           -> close_out out; x
    | exception e -> close_out out; raise e
  in
  begin match test_result with
  | Failure -> print_diff ~use_color ~file1:(File.Name.to_string filename) ~file2:target
  | Success -> Unix.unlink target
  | Error -> ()
  end;
  test_result
;;

let evaluate_tests ~use_color =
  convert_collector_tests (Expect_test_collector.tests_run ())
  |> List.map ~f:(process_group ~use_color)
  |> Test_result.combine_all
;;
