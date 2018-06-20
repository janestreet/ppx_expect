open Base
open Stdio
open Expect_test_common.Std

let fprintf = Out_channel.fprintf

module Saved_output = struct
  type t =
    | One of string
    | Many_distinct of string list

  let of_nonempty_list_exn outputs =
    let (_, rev_deduped_preserving_order) =
      List.fold outputs ~init:(Set.empty (module String), [])
        ~f:(fun (as_set, as_list) output ->
          if Set.mem as_set output
          then (as_set, as_list)
          else (Set.add as_set output, output::as_list)
        )
    in
    match List.rev rev_deduped_preserving_order with
    | []       -> failwith "Saved_output.of_nonempty_list_exn got an empty list"
    | [output] -> One output
    | outputs  -> Many_distinct outputs
  ;;

  let to_list = function
    | One s              -> [s]
    | Many_distinct many -> many
  ;;

  let merge t1 t2 = of_nonempty_list_exn (to_list t1 @ to_list t2)
end

module Test_outcome = struct
  module Expectations = struct
    type t = Fmt.t Cst.t Expectation.t Map.M(File.Location).t
    [@@deriving compare]

    let equal = [%compare.equal: t]
  end

  type t =
    { expectations             : Expectations.t
    ; uncaught_exn_expectation : Fmt.t Cst.t Expectation.t option
    ; saved_output             : Saved_output.t Map.M(File.Location).t
    ; trailing_output          : Saved_output.t
    ; uncaught_exn             : Saved_output.t option
    ; upon_unreleasable_issue  : Expect_test_config.Upon_unreleasable_issue.t
    }

  let merge_exn t
        { expectations
        ; uncaught_exn_expectation
        ; saved_output
        ; trailing_output
        ; uncaught_exn
        ; upon_unreleasable_issue
        } =
    if not (Expectations.equal t.expectations expectations)
    then failwith "merging tests of different expectations";
    if not (Expect_test_config.Upon_unreleasable_issue.equal
              t.upon_unreleasable_issue
              upon_unreleasable_issue)
    then failwith "merging tests of different [Upon_unreleasable_issue]";
    if not ([%compare.equal: Fmt.t Cst.t Expectation.t option] t.uncaught_exn_expectation
              uncaught_exn_expectation)
    then failwith "merging tests of different uncaught exception expectations";
    { expectations
    ; uncaught_exn_expectation
    ; saved_output =
        Map.merge t.saved_output saved_output ~f:(fun ~key:_ -> function
          | `Left x      -> Some x
          | `Right x     -> Some x
          | `Both (x, y) -> Some (Saved_output.merge x y))
    ; uncaught_exn =
        (match t.uncaught_exn, uncaught_exn with
         | None, None -> None
         | Some x, None | None, Some x -> Some x
         | Some x, Some y -> Some (Saved_output.merge x y))
    ; trailing_output = Saved_output.merge t.trailing_output trailing_output
    ; upon_unreleasable_issue
    }
  ;;
end

module Test_correction = struct
  module Node_correction = struct
    type t =
      | Collector_never_triggered
      | Correction of Fmt.t Cst.t Expectation.Body.t
  end

  module Uncaught_exn = struct
    type t =
      | Match
      | Without_expectation of Fmt.t Cst.t Expectation.Body.t
      | Correction          of Fmt.t Cst.t Expectation.t * Fmt.t Cst.t Expectation.Body.t
      | Unused_expectation  of Fmt.t Cst.t Expectation.t
  end

  type t =
    { location        : File.Location.t
    ; (* In the order of the file *)
      corrections     : (Fmt.t Cst.t Expectation.t * Node_correction.t) list
    ; uncaught_exn    : Uncaught_exn.t
    ; trailing_output : Fmt.t Cst.t Expectation.Body.t Reconcile.Result.t
    }

  let map_corrections t ~f =
    { location = t.location
    ; corrections = List.map t.corrections ~f:(fun (e, c) ->
        (e, match c with
         | Collector_never_triggered -> c
         | Correction body -> Correction (Expectation.Body.map_pretty body ~f)))
    ; uncaught_exn =
        (match t.uncaught_exn with
         | Match | Unused_expectation _ as x -> x
         | Without_expectation body ->
           Without_expectation (Expectation.Body.map_pretty body ~f)
         | Correction (e, body) ->
           Correction (e, Expectation.Body.map_pretty body ~f))
    ; trailing_output =
        Reconcile.Result.map t.trailing_output ~f:(Expectation.Body.map_pretty ~f)
    }
  ;;

  let compare_locations a b = compare a.location.line_number b.location.line_number

  let make ~location ~corrections ~uncaught_exn ~trailing_output : t Reconcile.Result.t =
    if List.is_empty corrections &&
       (match trailing_output with
        | Reconcile.Result.Match -> true
        | _ -> false) &&
       (match uncaught_exn with
        | Uncaught_exn.Match -> true
        | _ -> false) then
      Match
    else
      Correction { location; corrections; uncaught_exn; trailing_output }
  ;;
end

let indentation_at file_contents (loc : File.Location.t) =
  let n = ref loc.line_start in
  while Char.equal file_contents.[!n] ' ' do Int.incr n done;
  !n - loc.line_start
;;

let did_not_reach_this_program_point = "DID NOT REACH THIS PROGRAM POINT"

let evaluate_test ~file_contents ~(location : File.Location.t)
      ~allow_output_patterns (test : Test_outcome.t) =
  let cr_for_multiple_outputs ~cr_body outputs =
    let prefix =
      Expect_test_config.Upon_unreleasable_issue.comment_prefix
        test.upon_unreleasable_issue
    in
    let cr = Printf.sprintf "(* %sexpect_test: %s *)" prefix cr_body in
    let sep = String.init (String.length cr) ~f:(fun _ -> '=') in
    List.intersperse (cr::outputs) ~sep
    |> String.concat ~sep:"\n"
  in
  let corrections =
    Map.to_alist test.expectations
    |> List.filter_map ~f:(fun (location, (expect:Fmt.t Cst.t Expectation.t)) ->
      let correction_for actual =
        let default_indent = indentation_at file_contents expect.body_location in
        match
          Reconcile.expectation_body
            ~expect:expect.body
            ~actual
            ~default_indent
            ~pad_single_line:(Option.is_some expect.tag)
            ~allow_output_patterns
        with
        | Match        -> None
        | Correction c -> Some (expect, Test_correction.Node_correction.Correction c)
      in
      match Map.find test.saved_output location with
      | None -> begin
          match
            Reconcile.expectation_body
              ~expect:(Pretty (Single_line
                                 { leading_blanks  = ""
                                 ; trailing_spaces = ""
                                 ; orig = did_not_reach_this_program_point
                                 ; data = Literal did_not_reach_this_program_point
                                 }))
              ~actual:(match expect.body with
                | Pretty x -> Cst.to_string x
                | Exact  x -> x)
              ~default_indent:0
              ~pad_single_line:false
              ~allow_output_patterns:false
          with
          | Match -> None
          | Correction _ ->
            Some (expect, Test_correction.Node_correction.Collector_never_triggered)
        end
      | Some (One actual) -> correction_for actual
      | Some (Many_distinct outputs) ->
        let matches_expectation output = Option.is_none (correction_for output) in
        if List.for_all outputs ~f:matches_expectation
        then None
        else
          cr_for_multiple_outputs outputs
            ~cr_body:"Collector ran multiple times with different outputs"
          |> correction_for)
  in

  let trailing_output =
    let indent = location.start_pos - location.line_start + 2 in
    let actual =
      match test.trailing_output with
      | One actual -> actual
      | Many_distinct outputs ->
        cr_for_multiple_outputs outputs
          ~cr_body:"Test ran multiple times with different trailing outputs"
    in
    Reconcile.expectation_body
      ~expect:(Pretty Cst.empty)
      ~actual
      ~default_indent:indent
      ~pad_single_line:true
      ~allow_output_patterns
  in

  let uncaught_exn : Test_correction.Uncaught_exn.t =
    match test.uncaught_exn with
    | None -> begin
        match test.uncaught_exn_expectation with
        | None -> Match
        | Some e -> Unused_expectation e
      end
    | Some x ->
      let indent = location.start_pos - location.line_start in
      let actual =
        match x with
        | One actual -> actual
        | Many_distinct outputs ->
          cr_for_multiple_outputs outputs
            ~cr_body:"Test ran multiple times with different uncaught exceptions"
      in
      let expect =
        match test.uncaught_exn_expectation with
        | None -> Expectation.Body.Pretty Cst.empty
        | Some e -> e.body
      in
      match
        Reconcile.expectation_body
          ~expect
          ~actual
          ~default_indent:indent
          ~pad_single_line:true
          ~allow_output_patterns
      with
      | Match -> Match
      | Correction c ->
        match test.uncaught_exn_expectation with
        | None -> Without_expectation c
        | Some e -> Correction (e, c)
  in

  Test_correction.make ~location ~corrections ~uncaught_exn ~trailing_output
;;

type mode = Inline_expect_test | Toplevel_expect_test

let output_slice out s a b =
  Out_channel.output_string out (String.sub s ~pos:a ~len:(b - a))
;;

let rec output_semi_colon_if_needed oc file_contents pos =
  if pos >= 0 then
    match file_contents.[pos] with
    | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' ->
      output_semi_colon_if_needed oc file_contents (pos - 1)
    | ';' -> ()
    | _ -> Out_channel.output_char oc ';'
;;

let split_lines s = String.split s ~on:'\n'

let output_corrected oc ~file_contents ~mode test_corrections =
  let id_and_string_of_body : _ Expectation.Body.t -> string * string = function
    | Exact  x -> ("expect_exact", x)
    | Pretty x -> ("expect", Cst.to_string x)
  in
  let output_body oc tag body =
    match tag with
    | None ->
      fprintf oc "\"%s\""
        (String.concat ~sep:"\n" (split_lines body |> List.map ~f:String.escaped))
    | Some tag ->
      let tag = Choose_tag.choose ~default:tag body in
      fprintf oc "{%s|%s|%s}" tag body tag
  in
  let ofs =
    List.fold_left test_corrections ~init:0
      ~f:(fun ofs (test_correction : Test_correction.t) ->
        let ofs =
          List.fold_left test_correction.corrections ~init:ofs
            ~f:(fun ofs (e, correction) ->
              match (correction : Test_correction.Node_correction.t) with
              | Collector_never_triggered ->
                output_slice oc file_contents ofs e.Expectation.body_location.start_pos;
                output_body oc e.tag
                  (Printf.sprintf " %s " did_not_reach_this_program_point);
                e.body_location.end_pos
              | Correction c ->
                let id, body = id_and_string_of_body c in
                output_slice oc file_contents ofs e.extid_location.start_pos;
                Out_channel.output_string oc id;
                output_slice oc file_contents e.extid_location.end_pos
                  e.body_location.start_pos;
                output_body oc e.tag body;
                e.body_location.end_pos)
        in
        let ofs =
          match test_correction.trailing_output with
          | Match -> ofs
          | Correction c ->
            let loc = test_correction.location in
            output_slice oc file_contents ofs loc.end_pos;
            if match mode with Inline_expect_test -> true | _ -> false then
              output_semi_colon_if_needed oc file_contents loc.end_pos;
            let id, body = id_and_string_of_body c in
            (match mode with
             | Inline_expect_test ->
               let indent = loc.start_pos - loc.line_start + 2 in
               fprintf oc "\n%*s[%%%s " indent "" id
             | Toplevel_expect_test ->
               if loc.end_pos = 0 || Char.(<>) file_contents.[loc.end_pos - 1] '\n' then
                 Out_channel.output_char oc '\n';
               fprintf oc "[%%%%%s" id);
            output_body oc (Some "") body;
            fprintf oc "]";
            loc.end_pos
        in
        match test_correction.uncaught_exn with
        | Match -> ofs
        | Unused_expectation e ->
          (* Unfortunately, the OCaml parser doesn't give us the location of the whole
             extension point, so we have to find the square brackets ourselves :( *)
          let start = ref e.extid_location.start_pos in
          while not (Char.equal file_contents.[!start] '[') do Int.decr start done;
          output_slice oc file_contents ofs !start;
          let ofs = ref e.body_location.end_pos in
          while not (Char.equal file_contents.[!ofs] ']') do Int.incr ofs done;
          Int.incr ofs;
          while !ofs < String.length file_contents &&
                match file_contents.[!ofs] with
                | '\t' | '\011' | '\012' | '\r' | ' ' -> true
                | _ -> false
          do
            Int.incr ofs
          done;
          if !ofs < String.length file_contents &&
             Char.equal file_contents.[!ofs] '\n' then
            Int.incr ofs;
          !ofs
        | Without_expectation c ->
          let loc = test_correction.location in
          output_slice oc file_contents ofs loc.end_pos;
          let indent = loc.start_pos - loc.line_start in
          fprintf oc "\n%*s[@@expect.uncaught_exn " indent "";
          output_body oc (Some "") (snd (id_and_string_of_body c));
          fprintf oc "]";
          loc.end_pos
        | Correction (e, c) ->
          output_slice oc file_contents ofs e.body_location.start_pos;
          output_body oc e.tag (snd (id_and_string_of_body c));
          e.body_location.end_pos)
  in
  output_slice oc file_contents ofs (String.length file_contents)
;;

let write_corrected ~file ~file_contents ~mode test_corrections =
  Out_channel.with_file file ~f:(fun oc ->
    output_corrected oc ~file_contents ~mode
      (List.sort test_corrections ~compare:Test_correction.compare_locations))
;;
