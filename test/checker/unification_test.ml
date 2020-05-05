open Lambe.Ast.Type
open Lambe.Checker.Unification

let unification_error = Alcotest.testable Lambe.Render.Unification.pp ( = )

let lambe_type = Alcotest.testable Lambe.Render.Type.pp ( = )

let unify_type = Alcotest.(result (list (pair string lambe_type)) unification_error)

let should_unify_same_type () =
  let expected = Ok []
  and computed = unify (Variable "a") (Variable "a") in
  Alcotest.(check unify_type) "should_unify_ident_type" expected computed

let should_unify_apply_type () =
  let expected = Ok [ "x", Variable "Int"; "y", Variable "String" ]
  and computed = unify (Apply (Variable "x", Variable "y")) (Apply (Variable "Int", Variable "String")) in
  Alcotest.(check unify_type) "should_unify_apply_type" expected computed

let should_not_unify_apply_type_cyclic () =
  let expected = Error (CyclicUnification (Variable "x", Apply (Variable "Int", Variable "x")))
  and computed = unify (Variable "x") (Apply (Variable "Int", Variable "x")) in
  Alcotest.(check unify_type) "should_not_unify_apply_type_cyclic" expected computed

let test_cases =
  ( "Unification"
  , let open Alcotest in
    [
      test_case "Should unify same type" `Quick should_unify_same_type
    ; test_case "Should unify apply type" `Quick should_unify_apply_type
    ; test_case "Should not unify apply type (cyclic)" `Quick should_not_unify_apply_type_cyclic
    ] )
