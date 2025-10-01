open! Base
open! Portable
open Ppx_expect_runtime_types [@@alert "-ppx_expect_runtime_types"]
open Basement.Blocking_sync [@@alert "-deprecated"]

module Correction = struct
  type t =
    | New_payload : [< Test_spec.Behavior_type.t ] Test_spec.t * Output.Reconciled.t -> t
    | Unreachable : [ `Expect ] Test_spec.t -> t

  (** [Some (loc, patch)] if [correction] warrants inserting [patch] into the rewritten
      file at [loc], [None] if no change is needed from [correction]. *)
  let to_patch_opt ~(expect_node_formatting : Expect_node_formatting.t) correction =
    match correction with
    | New_payload
        ( { position
          ; behavior
          ; on_incorrect_output = T on_incorrect_output
          ; inconsistent_outputs_message = _
          ; payload_type = _
          }
        , test_output ) ->
      let whitespace =
        match position with
        | Insert { body_loc = { start_pos; start_bol; _ }; _ } ->
          (* [let_offset] is the space until the layer of indentation of the
             [let%expect_test] binding. *)
          let let_offset = start_pos - start_bol in
          (* The contents of the expect node are indented an additional two spaces past
             the node itself. *)
          let indent =
            let_offset
            +
            match on_incorrect_output.kind with
            | Extension -> expect_node_formatting.indent
            | Attribute -> 0
          in
          let whitespace = "\n" ^ String.make indent ' ' in
          whitespace
        | Overwrite _ -> ""
      in
      let tag =
        match behavior with
        | Expect { payload = { tag; _ }; on_unreachable = _; reachability = _ } -> tag
        | Unreachable _ -> String_node_format.Delimiter.default
      in
      let loc, correction =
        match position, on_incorrect_output with
        | ( Overwrite { payload = Some payload_loc; whole_node = _ }
          , { kind = Extension; hand = Longhand; name = _ } ) ->
          let correction =
            Output.to_formatted_payload ~tag test_output |> Payload.to_source_code_string
          in
          payload_loc, correction
        | (Overwrite { payload = _; whole_node = loc } | Insert { loc; body_loc = _ }), _
          ->
          ( loc
          , Output.to_source_code_string
              ~expect_node_formatting
              ~node_shape:(T on_incorrect_output)
              ~tag
              test_output )
      in
      Some (loc, whitespace ^ correction)
    | Unreachable
        { behavior = Expect { on_unreachable; payload = _; reachability = _ }
        ; on_incorrect_output = T on_incorrect_output
        ; position
        ; inconsistent_outputs_message = _
        ; payload_type = _
        } ->
      let loc = Test_spec.Insert_loc.loc position in
      (match on_unreachable with
       | Silent -> None
       | Delete -> Some (loc, "")
       | Replace_with replacement ->
         let prefix =
           match on_incorrect_output.kind with
           | Extension -> expect_node_formatting.extension_sigil
           | Attribute -> expect_node_formatting.attribute_sigil
         in
         Some (loc, Printf.sprintf "[%s%s]" prefix replacement))
  ;;

  let to_diffs ~expect_node_formatting ~original_file_contents correction =
    let safe_byte_get string i =
      if i >= 0 && i < String.length string then Some (String.get string i) else None
    in
    match to_patch_opt ~expect_node_formatting correction with
    | None -> []
    | Some (loc, diff) ->
      let ({ start_bol; start_pos; end_pos } : Compact_loc.t) = loc in
      let main_correction = [ loc, diff ] in
      (* Additional corrections necessary for producing correct formatting *)
      let additional_corrections =
        (* If deleting an [[@@expect.uncaught_exn]] attribute would
           leave an empty line, delete that line. *)
        let remove_empty_line_from_deleted_uncaught_exn =
          match correction with
          | Unreachable { on_incorrect_output = T { kind = Attribute; _ }; _ } ->
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
              ( { on_incorrect_output = T { kind = Extension; _ }
                ; position = Insert { body_loc; _ }
                ; _
                }
              , _ ) -> Some ({ body_loc with start_pos = body_loc.end_pos }, ";")
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

type one_output =
  { result : Output.Test_result.t
  ; raw : string
  }

type one_run =
  | Reached_with_output of one_output
  | Did_not_reach

type%fuelproof 'behavior inner : value mod contended portable =
  | Test :
      { expectation : ([< Test_spec.Behavior_type.t ] as 'behavior) Test_spec.t
      ; results : (one_run Queue.t, 'k) Capsule.Data.t
      ; mutex : 'k Mutex.t
      ; reached_this_run : bool Atomic.t
      }
      -> 'behavior inner

type%fuelproof t : value mod contended portable = T : 'behavior inner -> t

let to_correction
  ~expect_node_formatting
  ~cr_for_multiple_outputs
  (T (Test { expectation; results; mutex; reached_this_run = _ }))
  : Correction.t option
  =
  let results_list =
    (Mutex.with_lock mutex ~f:(fun password ->
       Capsule.Expert.access ~password ~f:(fun access ->
         let results = Capsule.Data.unwrap ~access results in
         { aliased = Queue.to_list results })
       [@nontail]))
      .aliased
  in
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
        (Comparable.lift ~f:(fun { result; _ } -> result) Output.Test_result.compare)
      outputs_list
  in
  let was_reached = List.is_empty unreached_list in
  let reachability_behavior =
    match expectation.behavior with
    | Expect { reachability; payload = _; on_unreachable = _ } -> reachability
    | Unreachable { reachability_of_corrected } -> reachability_of_corrected
  in
  let correction_for_single_result : Output.Test_result.t -> Correction.t option
    = function
    | Pass -> None
    | Fail received -> Some (New_payload (expectation, received))
  in
  match distinct_outputs, (was_reached, reachability_behavior) with
  | [], (_, _) ->
    (* The test was never reached *)
    (match expectation.behavior with
     | Unreachable _ -> None
     | Expect _ as behavior ->
       (* Error if an expect test was not reached *)
       Some (Unreachable (Test_spec.with_behavior expectation behavior)))
  | [ { result; _ } ], (true, _ | _, Can_reach) ->
    (* The test only produced one unique result and:
       - The test never failed to be reached OR
       - The test sometimes failed to be reached, but the test is marked as [Can_reach]
         (or rewrites to one marked as [Can_reach]) so that's OK
    *)
    correction_for_single_result result
  | _ :: _ :: _, _ | _, (false, Must_reach) ->
    (* The test results were inconsistent because:
       - The test was reached multiple times with different outputs OR
       - The test was sometimes reached and sometimes not, but the test rewrites to a
         test marked as [Must_reach]
    *)
    let outputs =
      results_list
      |> List.map ~f:(function
        | Reached_with_output { raw; _ } -> raw
        | Did_not_reach ->
          Printf.sprintf
            "<expect test ran without %s>"
            expectation.inconsistent_outputs_message)
    in
    cr_for_multiple_outputs ~output_name:expectation.inconsistent_outputs_message ~outputs
    |> Output.Formatter.apply (Test_spec.formatter ~expect_node_formatting expectation)
    |> Output.fail
    |> correction_for_single_result
;;

let compute_but_do_not_record_test_result
  ~expect_node_formatting
  ~test_output_raw
  (T (Test { expectation; _ }))
  : Output.Test_result.t * String_node_format.Delimiter.t
  =
  let test_output =
    Output.Formatter.apply
      (Test_spec.formatter ~expect_node_formatting expectation)
      test_output_raw
  in
  match expectation.behavior with
  | Unreachable _ -> Output.fail test_output, T (Tag "")
  | Expect { payload = { contents; tag }; on_unreachable = _; reachability = _ } ->
    Output.reconcile ~expected_output:contents ~test_output, tag
;;

let record_result
  ~test_output_raw
  ~failure_atomic
  (T (Test t))
  (result : Output.Test_result.t)
  =
  (match result with
   | Fail _ -> Atomic.set failure_atomic true
   | Pass -> ());
  Mutex.with_lock t.mutex ~f:(fun password ->
    Capsule.Expert.Data.iter t.results ~password ~f:(fun q ->
      Queue.enqueue q (Reached_with_output { result; raw = test_output_raw }))
    [@nontail]);
  Atomic.set t.reached_this_run true
;;

let of_expectation expectation =
  let (P key) = Capsule.Expert.create () in
  T
    (Test
       { expectation
       ; results = Capsule.Data.create Queue.create
       ; mutex = Mutex.create key
       ; reached_this_run = Atomic.make false
       })
;;

let loc (T (Test { expectation = { position; _ }; results = _; _ })) =
  Test_spec.Insert_loc.loc position
;;

let expectation_of_t
  (T (Test { expectation; results = _; mutex = _; reached_this_run = _ }))
  =
  match expectation.behavior with
  | Expect { payload = { contents; tag = _ }; on_unreachable = _; reachability = _ } ->
    Some contents
  | Unreachable _ -> None
;;

let record_end_of_run t =
  let (T (Test { expectation = _; results; mutex; reached_this_run })) = t in
  if not (Atomic.get reached_this_run)
  then
    Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Expert.Data.iter results ~password ~f:(fun q ->
        Queue.enqueue q Did_not_reach)
      [@nontail])
    [@nontail]
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
          Atomic.set t.reached_this_run false;
          Queue.enqueue tests_as_in_table (test_id, test);
          Set_to test);
      file);
    Queue.to_list tests_as_in_table
  ;;

  let process_each_file ~f =
    global_results_table
    |> Hashtbl.to_alist
    |> List.sort ~compare:(Comparable.lift ~f:fst String.compare)
    |> List.map ~f:(fun (filename, { expectations; postprocess }) ->
      let test_nodes = Hashtbl.data expectations in
      f ~filename ~test_nodes ~postprocess)
  ;;
end

module Create = struct
  let expect ~formatting_flexibility ~node_loc ~located_payload =
    of_expectation (Test_spec.expect ~formatting_flexibility ~node_loc ~located_payload)
  ;;

  let expect_exact ~formatting_flexibility ~node_loc ~located_payload =
    of_expectation
      (Test_spec.expect_exact ~formatting_flexibility ~node_loc ~located_payload)
  ;;

  let expect_if_reached ~formatting_flexibility ~node_loc ~located_payload =
    of_expectation
      (Test_spec.expect_if_reached ~formatting_flexibility ~node_loc ~located_payload)
  ;;

  let expectation ~formatting_flexibility ~node_loc ~located_payload =
    of_expectation
      (Test_spec.expectation ~formatting_flexibility ~node_loc ~located_payload)
  ;;

  let expect_unreachable ~node_loc =
    of_expectation (Test_spec.expect_unreachable ~node_loc)
  ;;

  let expectation_never_committed ~node_loc =
    of_expectation (Test_spec.expectation_never_committed ~node_loc)
  ;;
end

module For_mlt = struct
  let record_and_return_number_of_lines_in_correction
    ~expect_node_formatting
    ~failure_atomic
    ~test_output_raw
    t
    =
    let result, tag =
      compute_but_do_not_record_test_result ~expect_node_formatting ~test_output_raw t
    in
    record_result ~test_output_raw ~failure_atomic t result;
    match result with
    | Fail contents ->
      let correction =
        Output.to_formatted_payload ~tag contents |> Payload.to_source_code_string
      in
      Some (String.count ~f:(Char.equal '\n') correction + 1)
    | Pass -> None
  ;;

  let to_diffs ~cr_for_multiple_outputs ~expect_node_formatting ~original_file_contents t =
    match to_correction ~expect_node_formatting ~cr_for_multiple_outputs t with
    | None -> []
    | Some correction ->
      Correction.to_diffs correction ~expect_node_formatting ~original_file_contents
  ;;

  let loc = loc
  let expectation_of_t = expectation_of_t
end
