let should_provide_empty_freevar_from_ident () =
  let open Lambe_ast.Syntax in
  let open Lambe_checker in
  let expected = [] and computed = Freevar.from (Type.Ident "_") in
  Alcotest.(check (list string))
    "should_provide_empty_freevar_from_ident"
    expected
    computed

let should_provide_singleton_freevar_from_variable () =
  let open Lambe_ast.Syntax in
  let open Lambe_checker in
  let expected = [ "a" ] and computed = Freevar.from (Type.Variable "a") in
  Alcotest.(check (list string))
    "should_provide_singleton_freevar_from_variable"
    expected
    computed

let should_provide_empty_freevar_from_forall () =
  let open Lambe_ast.Syntax in
  let open Lambe_checker in
  let expected = []
  and computed =
    Freevar.from (Type.Forall ("a", Kind.Type, Type.Variable "a"))
  in
  Alcotest.(check (list string))
    "should_provide_empty_freevar_from_forall"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Freevar.from",
    [
      test_case
        "Should provide empty freevar from ident"
        `Quick
        should_provide_empty_freevar_from_ident;
      test_case
        "Should provide singleton freevar from variable"
        `Quick
        should_provide_singleton_freevar_from_variable;
      test_case
        "Should provide empty freevar from forall"
        `Quick
        should_provide_empty_freevar_from_forall;
    ] )
