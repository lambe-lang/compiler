open Lambe_ast.Type
open Lambe_checker

let lambe_type = Alcotest.testable Lambe_render.Type.pp ( = )

let should_substitute_in_variable () =
  let expected = Variable "Int"
  and computed =
    Substitution.substitute [ "a", Variable "Int" ] (Variable "a")
  in
  Alcotest.(check lambe_type) "should_substitute_in_variable" expected computed

let should_not_substitute_in_variable () =
  let expected = Variable "b"
  and computed =
    Substitution.substitute [ "a", Variable "Int" ] (Variable "b")
  in
  Alcotest.(check lambe_type)
    "should_not_substitute_in_variable" expected computed

let should_substitute_in_apply () =
  let expected = Apply (Variable "Int", Variable "String")
  and computed =
    Substitution.substitute [ "a", Variable "String" ]
      (Apply (Variable "Int", Variable "a"))
  in
  Alcotest.(check lambe_type) "should_substitute_in_apply" expected computed

let test_cases =
  ( "Subsitution"
  , let open Alcotest in
    [
      test_case "Should substitute in a variable" `Quick
        should_substitute_in_variable
    ; test_case "Should not substitute in a variable" `Quick
        should_not_substitute_in_variable
    ; test_case "Should not substitute in an apply" `Quick
        should_substitute_in_apply
    ] )
