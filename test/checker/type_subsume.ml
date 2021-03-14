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

let test_case_007 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- ( data "C" [ "h", v "a"; "t", v "b" ]
       <? data "C" [ "t", v "b"; "h", v "a" ] )
         Variables.create
  in
  Alcotest.(check bool)
    "should accept data C (h:a) (t:b) <? data C (t:b) (h:a)" expected computed

let test_case_008 () =
  let expected = true
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (data "C" [ "h", v "a"; "t", v "b" ] <? data "C" [ "t", v "b" ])
         Variables.create
  in
  Alcotest.(check bool)
    "should accept data C (h:a) (t:b) <? data C (t:b)" expected computed

let test_case_009 () =
  let expected = false
  and computed, _ =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (data "C" [ "t", v "b" ] <? data "C" [ "h", v "a"; "t", v "b" ])
         Variables.create
  in
  Alcotest.(check bool)
    "should reject data C (t:b) <? data C (h:a) (t:b)" expected computed

let test_cases =
  let open Alcotest in
  ( "Type subsume"
  , [
      test_case "Accept a <? a" `Quick test_case_000
    ; test_case "Reject a <? b" `Quick test_case_001
    ; test_case "Accept a <? a | b" `Quick test_case_002
    ; test_case "Accept a <? b | a" `Quick test_case_003
    ; test_case "Accept b | a -> a <? a -> a" `Quick test_case_004
    ; test_case "Accept b | a @> a <? a @> a" `Quick test_case_005
    ; test_case "Accept a | b <? b | a" `Quick test_case_006
    ; test_case "Accept data C (h:a) (t:b) <? data C (t:b) (h:a)" `Quick
        test_case_007
    ; test_case "Accept data C (h:a) (t:b) <? data C (t:b)" `Quick test_case_008
    ; test_case "Reject data C (t:b) <? data C (h:a) (t:b)" `Quick test_case_009
    ] )
