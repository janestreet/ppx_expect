(* Shadow the runtime so that we can see if we enter the body of the test functor *)
module Ppx_expect_runtime = struct
  include Ppx_expect_runtime

  module Make_test_block (C : Expect_test_config_types.S) = struct
    let () = failwith "entered test functor"

    include Make_test_block (C)
  end
end

let%expect_test "to_drop" =
  print_endline "OK";
  [%expect {| NOT OK |}]
;;

let the_num = 42
