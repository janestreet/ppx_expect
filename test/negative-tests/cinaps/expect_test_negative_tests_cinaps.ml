open! Base

let print_newline () = Stdio.print_endline ""
let print_s sexp = Stdio.print_string (Sexp_pretty.sexp_to_string sexp)

let generate filenames =
  let filenames = List.sort filenames ~compare:String.compare in
  let targets =
    List.concat
      [ List.map filenames ~f:(fun filename -> filename ^ ".corrected")
      ; [ "test-output" ]
      ]
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
            {|
rm -f *.ml.corrected 2>/dev/null
! %{first_dep} -no-color > test-output 2>&1
for f in *.ml.corrected
do
  %{root}/bin/apply-style \
    -directory-config jbuild \
    -original-file $(basename $f .corrected) \
    - < $f > $f.tmp
  mv $f.tmp $f
done
|}
        }];
  List.iter targets ~f:(fun target ->
    let deps = [ target ^ ".expected"; target ] in
    print_newline ();
    print_s
      [%sexp `alias { name = "runtest"; deps : string list; action = "diff -a %{deps}" }]);
  print_newline ()
;;
