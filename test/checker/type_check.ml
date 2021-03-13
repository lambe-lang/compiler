open Lambe_checker.Gamma
open Lambe_checker.Type.Checker.Operator
open Dsl
open Dsl.Types

let test_case_000 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", star ] + empty) |- (v "a" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a :? *" expected computed

let test_case_001 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" |-> v "a" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a -> a :? *" expected computed

let test_case_002 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" |@> v "a" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a @> a :? *" expected computed

let test_case_003 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", star |-> star ] + empty)
    |- (v "a" <*> v "a" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a a :? *" expected computed

let test_case_004 () =
  let expected = false
  and computed =
    Gamma.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" <*> v "a" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should reject a a :? *" expected computed

let test_case_005 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", trait [ "n", star ] ] + empty)
    |- (v "a" @ "n" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a.n :? {n:*}" expected computed

let test_case_006 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <|> v "b" <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept a | b :? *" expected computed

let test_case_007 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "b", star ] + empty)
    |- (forall ("x", Gamma.star) (v "b") <:?> Gamma.(star |-> star))
  in
  Alcotest.(check bool) "should accept forall (x:*).b :? *->*" expected computed

let test_case_008 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "b", star ] + empty)
    |- (exists ("x", Gamma.star) (v "b") <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept exists (x:*).b :? *->*" expected computed

let test_case_009 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "b", star ] + empty)
    |- (mu "x" (v "x") <:?> Gamma.star)
  in
  Alcotest.(check bool) "should accept mu (x).x :? *" expected computed

let test_case_010 () =
  let expected = true
  and computed =
    Gamma.(Helpers.k_set [ "b", star |-> star ] + empty)
    |- (mu "x" (v "b" <*> v "x") <:?> Gamma.star)
  in
  Alcotest.(check bool)
    "should accept mu (x).(b x) :? * with b:*->*" expected computed

let test_cases =
  let open Alcotest in
  ( "Type check"
  , [
      test_case "Accept a :? *" `Quick test_case_000
    ; test_case "Accept a -> a :? *" `Quick test_case_001
    ; test_case "Accept a @> a :? *" `Quick test_case_002
    ; test_case "Accept a a :? * with a:*->*" `Quick test_case_003
    ; test_case "Reject a a :? * with a:*" `Quick test_case_004
    ; test_case "Accept a.n :? {n:*}" `Quick test_case_005
    ; test_case "Accept a | b :? *" `Quick test_case_006
    ; test_case "Accept forall (x:*).b :? *->*" `Quick test_case_007
    ; test_case "Accept exists (x:*).b :? *" `Quick test_case_008
    ; test_case "Accept mu (x).x :? *" `Quick test_case_009
    ; test_case "Accept mu (x).(b x) :? * with b:*->*" `Quick test_case_010
    ] )
