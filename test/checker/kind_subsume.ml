open Lambe_checker.Kind
open Lambe_checker.Kind.Checker.Operator
open Dsl.Kinds

let test_case_0010 () =
  let expected = true
  and computed = Checker.subsume star star in
  Alcotest.(check bool) "should accept * <? *" expected computed

let test_case_0020 () =
  let expected = false
  and computed = star |-> star <? star in
  Alcotest.(check bool) "should reject * -> * <? *" expected computed

let test_case_0030 () =
  let expected = false
  and computed = star <? (star |-> star) in
  Alcotest.(check bool) "should reject * <? * -> *" expected computed

let test_case_0040 () =
  let expected = true
  and computed = trait [ "n", star; "m", star ] <? star in
  Alcotest.(check bool) "should accept {..} <? *" expected computed

let test_case_0050 () =
  let expected = true
  and computed = trait [ "n", star; "m", star ] <? trait [ "n", star ] in
  Alcotest.(check bool) "should accept {n:*,m:*} <? {n:*}" expected computed

let test_case_0060 () =
  let expected = false
  and computed = trait [ "n", star ] <? trait [ "n", star; "m", star ] in
  Alcotest.(check bool) "should reject {n:*} <? {n:*,m:*}" expected computed

let test_case_0070 () =
  let expected = true
  and computed =
    trait [ "n", star ] |-> star <? (trait [ "n", star; "m", star ] |-> star)
  in
  Alcotest.(check bool)
    "should accept {n:*} -> * <? {n:*,m:*} -> *" expected computed

let test_case_0080 () =
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
      test_case "Accept * < *" `Quick test_case_0010
    ; test_case "Reject * -> * < *" `Quick test_case_0020
    ; test_case "Reject * < * -> *" `Quick test_case_0030
    ; test_case "Accept {..} < *" `Quick test_case_0040
    ; test_case "Accept {n:*,m:*} < {n:*}" `Quick test_case_0050
    ; test_case "Reject {n:*} < {n:*,m:*}" `Quick test_case_0060
    ; test_case "Accept {n:*} -> * < {n:*,m:*} -> *" `Quick test_case_0070
    ; test_case "Reject {n:*,m:*} -> * < {n:*} -> *" `Quick test_case_0080
    ] )
