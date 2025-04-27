[@@@warning "-32"]

(* $MDX part-begin=zero_alloc *)
[@@@zero_alloc all]

let doesn't_allocate () = ()
let[@zero_alloc ignore] allocates () = ref 1

let%expect_test "doesn't allocate" =
  let () = doesn't_allocate () in
  ()
;;

let%expect_test[@zero_alloc ignore] "allocates" =
  let r = allocates () in
  r := 2;
  ()
;;

let%expect_test "allocates" =
  let r = allocates () in
  r := 2;
  ()
[@@zero_alloc ignore]
;;

(* $MDX part-end *)

let%expect_test "allocates" =
  let r = allocates () in
  r := 2;
  Printexc.record_backtrace false;
  ignore (failwith "dummy error" : unit)
[@@zero_alloc ignore] [@@expect.uncaught_exn {| (Failure "dummy error") |}]
;;
