open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Type.Checker.Operator
open Dsl
open Dsl.Types

let test_case_000 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" <? v "a") Variables.create
  in
  Alcotest.(check bool) "should accept a <? a" expected computed

let test_case_001 () =
  let expected = false
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? v "b") Variables.create
  in
  Alcotest.(check bool) "should accept a <? b" expected computed

let test_case_002 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? (v "a" <|> v "b")) Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_003 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? (v "b" <|> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_004 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" |-> v "a" <? (v "a" |-> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b -> a <? a -> a" expected computed

let test_case_005 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" |@> v "a" <? (v "a" |@> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b @-> a <? a @-> a" expected computed

let test_case_006 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" <? (v "a" <|> v "b")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b <? b | a" expected computed

let test_cases =
  let open Alcotest in
  ( "Type subsume"
  , [
      test_case "Should accept a <? a" `Quick test_case_000
    ; test_case "Should reject a <? b" `Quick test_case_001
    ; test_case "Should accept a <? a | b" `Quick test_case_002
    ; test_case "Should accept a <? b | a" `Quick test_case_003
    ; test_case "Should accept b | a -> a <? a -> a" `Quick test_case_004
    ; test_case "Should accept b | a @> a <? a @> a" `Quick test_case_005
    ; test_case "Should accept b | a <? a | b" `Quick test_case_006
    ] )
