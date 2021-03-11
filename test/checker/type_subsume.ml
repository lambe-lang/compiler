open Lambe_ast
open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Type

let should_accept_variable_subsume_same_variable () =
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", Kind.Type () ] + create)
      (Variable ("a", ()))
      (Variable ("a", ()))
      Variables.create
  in
  Alcotest.(check bool) "should accept a < a" expected computed

let should_reject_variable_subsume_another_variable () =
  let expected = false
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", Kind.Type (); "b", Kind.Type () ] + create)
      (Variable ("a", ()))
      (Variable ("b", ()))
      Variables.create
  in
  Alcotest.(check bool) "should accept a < b" expected computed

let should_accept_variable_subsume_variables_union_left () =
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", Kind.Type (); "b", Kind.Type () ] + create)
      (Variable ("a", ()))
      (Union (Variable ("a", ()), Variable ("b", ()), ()))
      Variables.create
  in
  Alcotest.(check bool) "should accept a < b | a" expected computed

let should_accept_variable_subsume_variables_union_right () =
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", Kind.Type (); "b", Kind.Type () ] + create)
      (Variable ("a", ()))
      (Union (Variable ("b", ()), Variable ("a", ()), ()))
      Variables.create
  in
  Alcotest.(check bool) "should accept a < b | a" expected computed

let test_cases =
  let open Alcotest in
  ( "Type subsume"
  , [
      test_case "Should accept a < a" `Quick
        should_accept_variable_subsume_same_variable
    ; test_case "Should reject a < b" `Quick
        should_reject_variable_subsume_another_variable
    ; test_case "Should accept a < a | b" `Quick
        should_accept_variable_subsume_variables_union_left
    ; test_case "Should accept a < b | a" `Quick
        should_accept_variable_subsume_variables_union_right
    ] )
