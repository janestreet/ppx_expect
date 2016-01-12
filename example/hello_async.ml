open Core.Std

let%expect_test _ =
  let open Async.Std in
  Thread_safe.block_on_async_exn (fun () ->
    Deferred.List.iter ["hello, "; "world"; "!"] ~f:(fun s ->
      print_string s;
      Clock.after (sec 0.3)
    )
  );

  [%expect {| hello, world! |}]
