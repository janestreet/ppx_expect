module Expect_test_config = struct
  include Expect_test_config

  let upon_unreleasable_issue = `Warning_for_collector_testing
end

module M (S : sig
  val output : string
end) =
struct
  let%expect_test _ =
    print_string S.output;
    [%expect {| hello world |}];
    print_string S.output
  ;;

  let%expect_test _ =
    print_string S.output;
    if not (String.equal S.output "foo") then failwith "wrong output";
    [%expect {| foo |}]
  ;;

  let%expect_test _ =
    if String.equal S.output "bar" then print_string S.output else failwith "wrong output";
    [%expect.unreachable]
    [@@expect.uncaught_exn {| (Failure "wrong output") |}]
  ;;
end

module A = M (struct
  let output = "foo"
end)

module B = M (struct
  let output = "bar"
end)

module C = M (struct
  let output = "cat"
end)
