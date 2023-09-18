module M (S : sig
  val foo : string
end) =
struct
  let%expect_test "similar passing" =
    print_string S.foo;
    [%expect {| foo |}]
  ;;

  let%expect_test "similar failing" =
    print_string S.foo;
    [%expect {| bar |}]
  ;;
end

module M1 = M (struct
  let foo = "foo"
end)

module M2 = M (struct
  let foo = "\n\nfoo\n\n"
end)
