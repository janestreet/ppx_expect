module type S = Expect_test_config_intf.S
module type Expect_test_config = Expect_test_config_intf.Expect_test_config

module Upon_unreleasable_issue = struct
  include Expect_test_config_intf.Upon_unreleasable_issue

  let equal t1 t2 = t1 = t2

  let comment_prefix = function
    | `CR -> "CR "
    | `Warning_for_collector_testing -> ""
  ;;

  let message_when_expectation_contains_backtrace t =
    Printf.sprintf
      {|
(* %sexpect_test_collector: This test expectation appears to contain a backtrace.
   This is strongly discouraged as backtraces are fragile.
   Please change this test to not include a backtrace. *)

|}
      (comment_prefix t)
  ;;
end

module IO_run = struct
  type 'a t = 'a

  let return x = x
  let bind t ~f = f t
end

module IO_flush = struct
  include IO_run

  let to_run t = t
end

let flush () = () (* the runtime already flushes [stdout] *)

let run f = f ()
let flushed () = true (* the runtime flushed [stdout] before calling this function *)

let upon_unreleasable_issue = `CR
