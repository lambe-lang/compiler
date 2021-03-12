open Lambe_ast
open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Type

module Dsl = struct
  let star = Kind.Type ()

  let v n = Type.Variable (n, ())

  let ( |-> ) t t' = Type.Arrow (t, t', ())

  let ( |@> ) t t' = Type.Invoke (t, t', ())

  let ( % ) t t' = Type.Apply (t, t', ())

  let ( <|> ) t t' = Type.Union (t, t', ())

  let ( @ ) t n = Type.Access (t, n, ())

  let forall (n, k) t = Type.Forall (n, k, t, ())

  let exists (n, k) t = Type.Exists (n, k, t, ())

  let mu n t = Type.Rec (n, t, ())

  let data c l = Type.Const (c, l, ())
end

let test_case_000 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star ] + create)
      (v "a") (v "a") Variables.create
  in
  Alcotest.(check bool) "should accept a <? a" expected computed

let test_case_001 () =
  let open Dsl in
  let expected = false
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "a") (v "b") Variables.create
  in
  Alcotest.(check bool) "should accept a <? b" expected computed

let test_case_002 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "a")
      (v "a" <|> v "b")
      Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_003 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "a")
      (v "b" <|> v "a")
      Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_004 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "b" <|> v "a" |-> v "a")
      (v "a" |-> v "a")
      Variables.create
  in
  Alcotest.(check bool) "should accept a | b -> a <? a -> a" expected computed

let test_case_005 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "b" <|> v "a" |@> v "a")
      (v "a" |@> v "a")
      Variables.create
  in
  Alcotest.(check bool) "should accept a | b @-> a <? a @-> a" expected computed

let test_case_006 () =
  let open Dsl in
  let expected = true
  and computed, _ =
    Checker.subsume
      (Helpers.k_set [ "a", star; "b", star ] + create)
      (v "b" <|> v "a")
      (v "a" <|> v "b")
      Variables.create
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
