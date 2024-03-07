open! Core

let print_newline () = Stdio.print_endline ""
let print_s sexp = Stdio.print_string (Sexp_pretty.sexp_to_string sexp)

let generate ~postprocess_test_output ~set_up_for_tests ~inline_test_args ~exclude_targets
  =
  let filenames =
    Filesystem_core.ls_dir File_path.dot
    |> List.map ~f:File_path.Part.to_string
    |> List.filter ~f:(fun filename ->
         String.is_suffix filename ~suffix:".ml"
         && not (List.mem exclude_targets filename ~equal:String.equal))
  in
  let filenames = List.sort filenames ~compare:String.compare in
  let targets =
    List.concat
      [ List.map filenames ~f:(fun filename -> filename ^ ".corrected")
      ; [ "test-output" ]
      ]
  in
  let set_up_string =
    match set_up_for_tests with
    | Some set_up_string -> set_up_string ^ " "
    | None -> ""
  in
  let postprocess_string =
    match postprocess_test_output with
    | Some postprocess_string -> postprocess_string
    | None -> "> test-output 2>&1"
  in
  let args_string =
    match inline_test_args with
    | Some args_string -> Printf.sprintf " %s -no-color " args_string
    | None -> " -no-color "
  in
  print_newline ();
  print_s
    [%sexp
      `rule
        { deps =
            [ "./inline_tests_runner"
            ; "./inline_tests_runner.exe"
            ; "%{root}/bin/apply-style"
            ; "jbuild"
            ; `glob_files "*.ml"
            ]
        ; targets : string list
        ; action : string =
            [%string_dedent
              {|
              >
              > rm -f *.ml.corrected 2>/dev/null
              > ! %{set_up_string}%{"%"}{first_dep}%{args_string}%{postprocess_string}
              > for f in *.ml.corrected
              > do
              >   %{"%"}{root}/bin/apply-style \
              >     -directory-config jbuild \
              >     -original-file $(basename $f .corrected) \
              >     - < $f > $f.tmp
              >   mv $f.tmp $f
              > done
              >
              |}]
        }];
  List.iter targets ~f:(fun target ->
    let deps = [ target ^ ".expected"; target ] in
    print_newline ();
    print_s
      [%sexp `alias { name = "runtest"; deps : string list; action = "diff -a %{deps}" }]);
  print_newline ()
;;
