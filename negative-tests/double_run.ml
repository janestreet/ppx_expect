let () = Printexc.record_backtrace false

module M() = struct
  let%expect_test _ = ()
end

module A = M()
module B = M()
