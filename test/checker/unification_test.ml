open Lambe.Ast.Type
open Lambe.Checker.Unification

let unification_error = Alcotest.testable Lambe.Render.Unification.pp ( = )

let lambe_type = Alcotest.testable Lambe.Render.Type.pp ( = )

let unify_type =
  Alcotest.(result (list (pair string lambe_type)) unification_error)

let should_unify_ident_type () =
  let expected = Ok []
  and computed = unify (Ident "a") (Ident "a") in
  Alcotest.(check unify_type) "should_unify_ident_type" expected computed

let should_not_unify_ident_type () =
  let expected = Error (CannotUnify (Ident "a", Ident "b"))
  and computed = unify (Ident "a") (Ident "b") in
  Alcotest.(check unify_type) "should_not_unify_ident_type" expected computed

let should_unify_arrow_type () =
  let expected = Ok [ "x", Ident "Int"; "y", Ident "String" ]
  and computed =
    unify
      (Arrow (Variable "x", Ident "String"))
      (Arrow (Ident "Int", Variable "y"))
  in
  Alcotest.(check unify_type) "should_unify_arrow_type" expected computed

let should_not_unify_arrow_type () =
  let expected = Error (CannotUnify (Ident "String", Ident "Int"))
  and computed =
    unify
      (Arrow (Variable "x", Ident "String"))
      (Arrow (Ident "Int", Variable "x"))
  in
  Alcotest.(check unify_type) "should_not_unify_arrow_type" expected computed

let should_not_unify_arrow_type_cyclic () =
  let expected =
    Error (CyclicUnification (Variable "x", Arrow (Ident "Int", Variable "x")))
  and computed = unify (Variable "x") (Arrow (Ident "Int", Variable "x")) in
  Alcotest.(check unify_type)
    "should_not_unify_arrow_type_cyclic" expected computed

let should_unify_apply_type () =
  let expected = Ok [ "x", Ident "Int"; "y", Ident "String" ]
  and computed =
    unify
      (Apply (Variable "x", Ident "String"))
      (Apply (Ident "Int", Variable "y"))
  in
  Alcotest.(check unify_type) "should_unify_apply_type" expected computed

let should_not_unify_apply_type () =
  let expected = Error (CannotUnify (Ident "String", Ident "Int"))
  and computed =
    unify
      (Apply (Variable "x", Ident "String"))
      (Apply (Ident "Int", Variable "x"))
  in
  Alcotest.(check unify_type) "should_not_unify_apply_type" expected computed

let should_not_unify_apply_type_cyclic () =
  let expected =
    Error (CyclicUnification (Variable "x", Apply (Ident "Int", Variable "x")))
  and computed = unify (Variable "x") (Apply (Ident "Int", Variable "x")) in
  Alcotest.(check unify_type)
    "should_not_unify_apply_type_cyclic" expected computed

let test_cases =
  ( "Unification"
  , let open Alcotest in
    [
      test_case "Should unify Ident type" `Quick should_unify_ident_type
    ; test_case "Should not unify ident type" `Quick should_not_unify_ident_type
    ; test_case "Should unify arrow type" `Quick should_unify_arrow_type
    ; test_case "Should not unify arrow type" `Quick should_not_unify_arrow_type
    ; test_case "Should not unify arrow type (cyclic)" `Quick
        should_not_unify_arrow_type_cyclic
    ; test_case "Should unify apply type" `Quick should_unify_apply_type
    ; test_case "Should not unify apply type" `Quick should_not_unify_apply_type
    ; test_case "Should not unify apply type (cyclic)" `Quick
        should_not_unify_apply_type_cyclic
    ] )
