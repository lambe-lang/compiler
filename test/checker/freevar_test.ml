let should_provide_empty_freevar_from_native () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = []
  and computed = Variables.free_vars Type.(Ident "Int") in
  Alcotest.(check (list string))
    "should_provide_empty_freevar_from_native" expected computed

let should_provide_empty_freevar_from_ident () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = []
  and computed = Variables.free_vars (Type.Ident "_") in
  Alcotest.(check (list string))
    "should_provide_empty_freevar_from_ident" expected computed

let should_provide_singleton_freevar_from_variable () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = [ "a" ]
  and computed = Variables.free_vars (Type.Variable "a") in
  Alcotest.(check (list string))
    "should_provide_singleton_freevar_from_variable" expected computed

let should_provide_empty_freevar_from_forall () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = []
  and computed =
    Variables.free_vars (Type.Forall ("a", Kind.Type, Type.Variable "a"))
  in
  Alcotest.(check (list string))
    "should_provide_empty_freevar_from_forall" expected computed

let should_provide_singleton_freevar_from_forall () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = [ "b" ]
  and computed =
    Variables.free_vars (Type.Forall ("a", Kind.Type, Type.Variable "b"))
  in
  Alcotest.(check (list string))
    "should_provide_singleton_freevars_from_forall" expected computed

let should_provide_two_freevar_from_apply () =
  let open Lambe_ast in
  let open Lambe_checker in
  let expected = [ "b"; "a" ]
  and computed =
    Variables.free_vars (Type.Apply (Type.Variable "b", Type.Variable "a"))
  in
  Alcotest.(check (list string))
    "should_provide_two_freevars_from_apply" expected computed

let test_cases =
  let open Alcotest in
  ( "Freevar"
  , [
      test_case "Should provide empty freevar from native" `Quick
        should_provide_empty_freevar_from_native
    ; test_case "Should provide empty freevar from ident" `Quick
        should_provide_empty_freevar_from_ident
    ; test_case "Should provide singleton freevar from variable" `Quick
        should_provide_singleton_freevar_from_variable
    ; test_case "Should provide empty freevar from forall" `Quick
        should_provide_empty_freevar_from_forall
    ; test_case "Should provide singleton freevar from forall" `Quick
        should_provide_singleton_freevar_from_forall
    ; test_case "Should provide two freevars from apply" `Quick
        should_provide_two_freevar_from_apply
    ] )
