open! Base

let current = ref None

let set ~filename_rel_to_project_root =
  match !current with
  | None -> current := Some filename_rel_to_project_root
  | Some current ->
    raise_s
      (Sexp.message
         "Expect_test_collector.set: there is already an active file"
         [ "old_file", sexp_of_string current
         ; "new_file", sexp_of_string filename_rel_to_project_root
         ])
;;

let unset () =
  match !current with
  | Some _ -> current := None
  | None ->
    raise_s (Sexp.message "Expect_test_collector.unset: there is no active file" [])
;;

let get () =
  match !current with
  | Some fn -> fn
  | None -> raise_s (Sexp.message "Expect_test_collector.get: there is no active file" [])
;;

let initial_dir =
  let dir_or_error = Or_error.try_with ~backtrace:true Stdlib.Sys.getcwd in
  lazy (Or_error.ok_exn dir_or_error)
;;

let absolute_path file =
  if Stdlib.Filename.is_relative file
  then Stdlib.Filename.concat (Lazy.force initial_dir) file
  else file
;;

let verify_that_file_is_current_exn ~line_number ~filename_rel_to_project_root =
  let registering_tests_for = get () in
  if not (String.equal filename_rel_to_project_root registering_tests_for)
  then
    Printf.ksprintf
      failwith
      "Trying to run an expect test from the wrong file.\n\
       - test declared at %s:%d\n\
       - trying to run it from %s\n"
      filename_rel_to_project_root
      line_number
      registering_tests_for
  else ()
;;
