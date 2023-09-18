open! Core

module M (X : Sexpable) = struct
  module N (Y : sig
    val x : X.t
  end) =
  struct
    let%expect_test "trailing output" =
      let sexp = X.sexp_of_t Y.x in
      print_s sexp
    ;;

    let%expect_test "error" =
      if String.length (string_of_sexp (X.sexp_of_t Y.x)) > 1
      then raise_s (Base.Sexp.message "sexp is too long" [ "input: ", X.sexp_of_t Y.x ])
    ;;
  end
end

module String_tests = M (String)

module Run_on_abc = String_tests.N (struct
  let x = "a\nb\nc"
end)
