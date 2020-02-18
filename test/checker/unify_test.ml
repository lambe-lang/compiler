open Lambe_ast.Type
open Lambe_checker

let lambe_type = Alcotest.testable Lambe_pp.Type.pp ( = )

let unify_type = Alcotest.(result (list (pair string lambe_type)) string)

let should_unify_native_type () =
  let expected = Ok []
  and computed = Types.unify (Native Int) (Native Int) in
  Alcotest.(check unify_type) "should_unify_native_type" expected computed

let should_unify_ident_type () =
  let expected = Ok []
  and computed = Types.unify (Ident "a") (Ident "a") in
  Alcotest.(check unify_type) "should_unify_ident_type" expected computed

let should_not_unify_ident_type () =
  let expected = Error "Unification fails"
  and computed = Types.unify (Ident "a") (Ident "b") in
  Alcotest.(check unify_type) "should_not_unify_ident_type" expected computed

let should_unify_arrow_type () =
  let expected = Ok [ "x", Native Int; "y", Native String ]
  and computed =
    Types.unify
      (Arrow (Variable "x", Native String))
      (Arrow (Native Int, Variable "y"))
  in
  Alcotest.(check unify_type) "should_unify_arrow_type" expected computed

let should_not_unify_arrow_type () =
  let expected = Error "Unification fails"
  and computed =
    Types.unify
      (Arrow (Variable "x", Native String))
      (Arrow (Native Int, Variable "x"))
  in
  Alcotest.(check unify_type) "should_not_unify_arrow_type" expected computed

let should_not_unify_arrow_type_cyclic () =
  let expected = Error "Cyclic unification"
  and computed =
    Types.unify (Variable "x") (Arrow (Native Int, Variable "x"))
  in
  Alcotest.(check unify_type)
    "should_not_unify_arrow_type_cyclic" expected computed

let should_unify_apply_type () =
  let expected = Ok [ "x", Native Int; "y", Native String ]
  and computed =
    Types.unify
      (Apply (Variable "x", Native String))
      (Apply (Native Int, Variable "y"))
  in
  Alcotest.(check unify_type) "should_unify_apply_type" expected computed

let should_not_unify_apply_type () =
  let expected = Error "Unification fails"
  and computed =
    Types.unify
      (Apply (Variable "x", Native String))
      (Apply (Native Int, Variable "x"))
  in
  Alcotest.(check unify_type) "should_not_unify_apply_type" expected computed

let should_not_unify_apply_type_cyclic () =
  let expected = Error "Cyclic unification"
  and computed =
    Types.unify (Variable "x") (Apply (Native Int, Variable "x"))
  in
  Alcotest.(check unify_type)
    "should_not_unify_apply_type_cyclic" expected computed

let test_cases =
  ( "Unification.unify"
  , let open Alcotest in
    [
      test_case "Should unify native type" `Quick should_unify_native_type
    ; test_case "Should unify ident type" `Quick should_unify_ident_type
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
