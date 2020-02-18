open Lambe_ast.Type
open Lambe_checker

let lambe_type = Alcotest.testable Lambe_pp.Type.pp ( = )

let should_substitute_in_variable () =
  let expected = Native Int
  and computed = Types.substitute [ "a", Native Int ] (Variable "a") in
  Alcotest.(check lambe_type) "should_substitute_in_variable" expected computed

let should_not_substitute_in_variable () =
  let expected = Variable "b"
  and computed = Types.substitute [ "a", Native Int ] (Variable "b") in
  Alcotest.(check lambe_type)
    "should_not_substitute_in_variable" expected computed

let should_substitute_in_arrow () =
  let expected = Arrow (Native Int, Native Int)
  and computed =
    Types.substitute [ "a", Native Int ] (Arrow (Native Int, Variable "a"))
  in
  Alcotest.(check lambe_type) "should_substitute_in_arrow" expected computed

let should_substitute_in_apply () =
  let expected = Apply (Native Int, Native Int)
  and computed =
    Types.substitute [ "a", Native Int ] (Apply (Native Int, Variable "a"))
  in
  Alcotest.(check lambe_type) "should_substitute_in_apply" expected computed

let test_cases =
  ( "Subsitution.subsitute"
  , let open Alcotest in
    [
      test_case "Should substitute in a variable" `Quick
        should_substitute_in_variable
    ; test_case "Should not substitute in a variable" `Quick
        should_not_substitute_in_variable
    ; test_case "Should not substitute in an arrow" `Quick
        should_substitute_in_arrow
    ; test_case "Should not substitute in an apply" `Quick
        should_substitute_in_apply
    ] )
