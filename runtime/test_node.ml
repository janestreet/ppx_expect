open! Base
open Types

module Correction = struct
  type t =
    | New_payload :
        ('output, [< Expectation.Behavior_type.t ]) Expectation.t * 'output
        -> t
    | Unreachable : ('output, [ `Expect ]) Expectation.t -> t

  (** [Some patch] if [correction] warrants inserting [patch] into the rewritten file,
      [None] if no change is needed from [correction]. *)
  let to_patch_opt ~(expect_node_formatting : Expect_node_formatting.t) correction =
    let (prefix_by_type : Expectation.Node_type.t -> string) = function
      | Extension -> expect_node_formatting.extension_sigil
      | Attribute -> expect_node_formatting.attribute_sigil
    in
    match correction with
    | New_payload
        ( { node_type
          ; position
          ; behavior
          ; payload_type
          ; on_incorrect_output
          ; inconsistent_outputs_message = _
          }
        , test_output ) ->
      let whitespace, indent =
        match position with
        | Insert { body_loc = { start_pos; start_bol; _ }; _ } ->
          (* [let_offset] is the space until the layer indentation of the
             [let%expect_test] binding. *)
          let let_offset = start_pos - start_bol in
          (* The contents of the expect node are indented an additional two spaces past
             the node itself. *)
          let indent =
            let_offset
            +
            match node_type with
            | Extension -> 2
            | Attribute -> 0
          in
          let whitespace = "\n" ^ String.make indent ' ' in
          whitespace, Some indent
        | Overwrite _ -> "", None
      in
      let prefix = prefix_by_type node_type in
      let payload =
        let module Payload_type = (val payload_type) in
        Payload_type.to_source_code_string
          ~expect_node_formatting
          ~indent
          (match behavior with
           | Expect { payload = { tag; _ }; on_unreachable = _; reachability = _ } ->
             { tag; contents = test_output }
           | Unreachable _ -> Payload.default test_output)
      in
      Some (Printf.sprintf "%s[%s%s %s]" whitespace prefix on_incorrect_output payload)
    | Unreachable
        { node_type
        ; behavior = Expect { on_unreachable; payload = _; reachability = _ }
        ; _
        } ->
      (match on_unreachable with
       | Silent -> None
       | Delete -> Some ""
       | Replace_with_unreachable ->
         let prefix = prefix_by_type node_type in
         Some (Printf.sprintf "[%sexpect.unreachable]" prefix))
  ;;

  let to_diffs ~expect_node_formatting ~original_file_contents correction =
    let safe_byte_get string i =
      if i >= 0 && i < String.length string then Some (String.get string i) else None
    in
    match to_patch_opt ~expect_node_formatting correction with
    | None -> []
    | Some diff ->
      let ({ start_bol; start_pos; end_pos } as loc : Compact_loc.t) =
        match correction with
        | New_payload (expectation, _) -> Expectation.loc expectation
        | Unreachable expectation -> Expectation.loc expectation
      in
      let main_correction = [ loc, diff ] in
      (* Additional corrections necessary for producing correct formatting *)
      let additional_corrections =
        (* If deleting an [[@@expect.uncaught_exn]] attribute would
           leave an empty line, delete that line. *)
        let remove_empty_line_from_deleted_uncaught_exn =
          match correction with
          | Unreachable { node_type = Attribute; _ } ->
            (match
               ( safe_byte_get original_file_contents (start_pos - 1)
               , safe_byte_get original_file_contents end_pos )
             with
             | Some '\n', (None | Some '\n') ->
               Some
                 ( { Compact_loc.start_pos = start_pos - 1
                   ; end_pos = start_pos
                   ; start_bol
                   }
                 , "" )
             | _ -> None)
          | _ -> None
        in
        (* Include the semicolon needed at the end of the body
           for a trailing [[%expect]] extension point. *)
        let add_semicolon_before_trailing_expect =
          match correction with
          | New_payload
              ({ node_type = Extension; position = Insert { body_loc; _ }; _ }, _) ->
            Some ({ body_loc with start_pos = body_loc.end_pos }, ";")
          | _ -> None
        in
        List.concat_map
          ~f:Option.to_list
          [ remove_empty_line_from_deleted_uncaught_exn
          ; add_semicolon_before_trailing_expect
          ]
      in
      additional_corrections @ main_correction
  ;;
end

type 'output one_output =
  { result : 'output Payload.Result.t
  ; raw : string
  }

type 'output one_run =
  | Reached_with_output of 'output one_output
  | Did_not_reach

type ('output, 'behavior) inner =
  | Test :
      { expectation :
          ('output, ([< Expectation.Behavior_type.t ] as 'behavior)) Expectation.t
      ; results : 'output one_run Queue.t
      ; mutable reached_this_run : bool
      }
      -> ('output, 'behavior) inner

type t = T : ('output, 'behavior) inner -> t

let to_correction
  ~cr_for_multiple_outputs
  (T (Test { expectation; results; reached_this_run = _ }))
  =
  let module Payload_type = (val expectation.payload_type) in
  let results_list = Queue.to_list results in
  let unreached_list, outputs_list =
    List.partition_map results_list ~f:(function
      | Did_not_reach -> First ()
      | Reached_with_output output -> Second output)
  in
  let distinct_outputs =
    (* Allow distinct raw outputs as long as their formatted [result]s
       are considered equivalent according to [Payload_type]. *)
    List.dedup_and_sort
      ~compare:
        (Comparable.lift
           ~f:(fun { result; _ } -> result)
           (Payload.Result.compare Payload_type.Contents.compare))
      outputs_list
  in
  let was_reached = List.is_empty unreached_list in
  let reachability_behavior =
    match expectation.behavior with
    | Expect { reachability; payload = _; on_unreachable = _ } -> reachability
    | Unreachable { reachability_of_corrected } -> reachability_of_corrected
  in
  match distinct_outputs, (was_reached, reachability_behavior) with
  | [], (_, _) ->
    (* The test was never reached *)
    (match expectation.behavior with
     | Unreachable _ -> None
     | Expect _ as behavior ->
       (* Error if an expect test was not reached *)
       Some (Correction.Unreachable (Expectation.with_behavior expectation behavior)))
  | [ { result; _ } ], (true, _ | _, Can_reach) ->
    (* The test only produced one unique result and:
       - The test never failed to be reached OR
       - The test sometimes failed to be reached, but the test is marked as [Can_reach]
         (or rewrites to one marked as [Can_reach]) so that's OK
    *)
    (match result with
     | Pass -> None
     | Fail received -> Some (New_payload (expectation, received)))
  | _ :: _ :: _, _ | _, (false, Must_reach) ->
    (* The test results were inconsistent because:
       - The test was reached multiple times with different outputs OR
       - The test was sometimes reached and sometimes not, but the test rewrites to a
         test marked as [Must_reach]
    *)
    let correction =
      let outputs =
        results_list
        |> List.map ~f:(function
             | Reached_with_output { raw; _ } -> raw
             | Did_not_reach ->
               Printf.sprintf
                 "<expect test ran without %s>"
                 expectation.inconsistent_outputs_message)
      in
      cr_for_multiple_outputs
        ~output_name:expectation.inconsistent_outputs_message
        ~outputs
      |> Payload_type.Contents.of_located_string ~loc:(Some (Expectation.loc expectation))
    in
    Some (New_payload (expectation, correction))
;;

let record_and_return_result
  : type output behavior.
    expect_node_formatting:Expect_node_formatting.t
    -> failure_ref:bool ref
    -> test_output_raw:string
    -> (output, behavior) inner
    -> output Payload.Result.t * Payload.Delimiter.t
  =
  fun ~expect_node_formatting
      ~failure_ref
      ~test_output_raw
      (Test ({ expectation; results; reached_this_run = _ } as t)) ->
  let module Payload_type = (val expectation.payload_type) in
  let test_output =
    Payload_type.Contents.of_located_string
      ~loc:(Some (Expectation.loc expectation))
      test_output_raw
  in
  let (result : _ Payload.Result.t), (tag : Payload.Delimiter.t) =
    match expectation.behavior with
    | Unreachable _ -> Fail test_output, Tag ""
    | Expect { payload = { contents; tag }; on_unreachable = _; reachability = _ } ->
      ( Payload_type.Contents.reconcile
          ~expect_node_formatting
          ~expected_output:contents
          ~test_output
      , tag )
  in
  (match result with
   | Fail _ -> failure_ref := true
   | Pass -> ());
  Queue.enqueue results (Reached_with_output { result; raw = test_output_raw });
  t.reached_this_run <- true;
  result, tag
;;

let of_expectation expectation =
  T (Test { expectation; results = Queue.create (); reached_this_run = false })
;;

let record_end_of_run t =
  let (T (Test { expectation = _; results; reached_this_run })) = t in
  if not reached_this_run then Queue.enqueue results Did_not_reach
;;

let record_result ~expect_node_formatting ~failure_ref ~test_output_raw (T inner) =
  ignore
    (record_and_return_result ~expect_node_formatting ~failure_ref ~test_output_raw inner
      : _ Payload.Result.t * Payload.Delimiter.t)
;;

module Global_results_table = struct
  type node = t
  type postprocess = node list Write_corrected_file.Patch_with_file_contents.t

  type file =
    { expectations : node Hashtbl.M(Expectation_id).t
    ; postprocess : postprocess
    }

  let global_results_table : file Hashtbl.M(String).t = Hashtbl.create (module String)

  let find_test ~absolute_filename ~(test_id : Expectation_id.t) =
    Hashtbl.find global_results_table absolute_filename
    |> Option.bind ~f:(fun { expectations; _ } -> Hashtbl.find expectations test_id)
    |> Option.value_exn
         ~error:
           (Error.of_string
              (Printf.sprintf
                 "Internal expect test bug: could not find test\nFile: %s\nID:   %d"
                 absolute_filename
                 (Expectation_id.to_int_exn test_id)))
  ;;

  let initialize_and_register_tests ~absolute_filename tests postprocess =
    let tests_as_in_table = Queue.create () in
    Hashtbl.update global_results_table absolute_filename ~f:(fun file ->
      let file =
        Option.value
          file
          ~default:{ expectations = Hashtbl.create (module Expectation_id); postprocess }
      in
      let tests = Hashtbl.of_alist_exn (module Expectation_id) tests in
      Hashtbl.merge_into
        ~src:tests
        ~dst:file.expectations
        ~f:(fun ~key:test_id new_test existing_test ->
        let (T (Test t) as test) = Option.value existing_test ~default:new_test in
        t.reached_this_run <- false;
        Queue.enqueue tests_as_in_table (test_id, test);
        Set_to test);
      file);
    Queue.to_list tests_as_in_table
  ;;

  let process_each_file ~f =
    global_results_table
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (filename, { expectations; postprocess }) ->
         let test_nodes = Hashtbl.data expectations in
         f ~filename ~test_nodes ~postprocess)
  ;;
end

module Create = struct
  let expect loc payload = of_expectation (Expectation.expect payload loc)
  let expect_exact loc payload = of_expectation (Expectation.expect_exact payload loc)
  let expect_unreachable loc = of_expectation (Expectation.expect_unreachable loc)
end

module For_mlt = struct
  let loc (T (Test { expectation; results = _; reached_this_run = _ })) =
    Expectation.loc expectation
  ;;

  let expectation_of_t
    ~expect_node_formatting
    (T (Test { expectation; results = _; reached_this_run = _ }))
    =
    match expectation.behavior with
    | Expect { payload = { contents; tag }; on_unreachable = _; reachability = _ } ->
      let module Payload_type = (val expectation.payload_type) in
      Some
        (Payload_type.Contents.to_source_code_string
           ~expect_node_formatting
           ~indent:None
           ~tag
           contents)
    | Unreachable _ -> None
  ;;

  let record_and_return_number_of_lines_in_correction
    ~expect_node_formatting
    ~failure_ref
    ~test_output_raw
    (T (Test inner))
    =
    match
      record_and_return_result
        ~expect_node_formatting
        ~failure_ref
        ~test_output_raw
        (Test inner)
    with
    | Fail contents, tag ->
      let module Payload_type = (val inner.expectation.payload_type) in
      let correction =
        Payload_type.to_source_code_string
          ~expect_node_formatting
          ~indent:None
          { contents; tag }
      in
      Some (String.count ~f:(Char.equal '\n') correction + 1)
    | Pass, _ -> None
  ;;

  let to_diffs ~cr_for_multiple_outputs ~expect_node_formatting ~original_file_contents t =
    match to_correction t ~cr_for_multiple_outputs with
    | None -> []
    | Some correction ->
      Correction.to_diffs correction ~expect_node_formatting ~original_file_contents
  ;;
end
