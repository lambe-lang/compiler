open Lambe_checker.Kind
open Lambe_checker.Kind.Checker.Operator
open Dsl.Gamma

let test_case_001 () =
  let expected = true
  and computed = Checker.subsume star star in
  Alcotest.(check bool) "should accept * <? *" expected computed

let test_case_002 () =
  let expected = false
  and computed = star |-> star <? star in
  Alcotest.(check bool) "should reject * -> * <? *" expected computed

let test_case_003 () =
  let expected = false
  and computed = star <? (star |-> star) in
  Alcotest.(check bool) "should reject * <? * -> *" expected computed

let test_case_004 () =
  let expected = true
  and computed = trait [ "n", star; "m", star ] <? star in
  Alcotest.(check bool) "should accept {..} <? *" expected computed

let test_case_005 () =
  let expected = true
  and computed = trait [ "n", star; "m", star ] <? trait [ "n", star ] in
  Alcotest.(check bool) "should accept {n:*,m:*} <? {n:*}" expected computed

let test_case_006 () =
  let expected = false
  and computed = trait [ "n", star ] <? trait [ "n", star; "m", star ] in
  Alcotest.(check bool) "should reject {n:*} <? {n:*,m:*}" expected computed

let test_case_007 () =
  let expected = true
  and computed =
    trait [ "n", star ] |-> star <? (trait [ "n", star; "m", star ] |-> star)
  in
  Alcotest.(check bool)
    "should accept {n:*} -> * <? {n:*,m:*} -> *" expected computed

let test_case_008 () =
  let expected = false
  and computed =
    trait [ "n", star; "m", star ] |-> star <? (trait [ "n", star ] |-> star)
  in
  Alcotest.(check bool)
    "should reject {n:*,m:*} -> * <? {n:*} -> *" expected computed

let test_cases =
  let open Alcotest in
  ( "Kind subsume"
  , [
      test_case "Accept * < *" `Quick test_case_001
    ; test_case "Reject * -> * < *" `Quick test_case_002
    ; test_case "Reject * < * -> *" `Quick test_case_003
    ; test_case "Accept {..} < *" `Quick test_case_004
    ; test_case "Accept {n:*,m:*} < {n:*}" `Quick test_case_005
    ; test_case "Reject {n:*} < {n:*,m:*}" `Quick test_case_006
    ; test_case "Accept {n:*} -> * < {n:*,m:*} -> *" `Quick test_case_007
    ; test_case "Reject {n:*,m:*} -> * < {n:*} -> *" `Quick test_case_008
    ] )
