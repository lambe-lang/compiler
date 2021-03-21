open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Expr.Checker.Operator
open Dsl.Exprs
module T = Dsl.Types
module K = Dsl.Kinds

let test_case_000 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "int", K.star ] + Helpers.s_set [ "a", T.v "int" ]
    |- (v "a" <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool) "should accept a :? int" expected computed

let test_case_001 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "int", K.star ]
    |- (lambda "x" (v "x") <:?> T.(v "int" |-> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept { x -> x } :? int -> int" expected computed

let test_case_002 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "int", K.star ]
    |- (zeta (v "self") <:?> T.(v "int" |@> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept {> self } :? int |@> int" expected computed

let test_cases =
  let open Alcotest in
  ( "Expression check"
  , [
      test_case "Accept a :? int" `Quick test_case_000
    ; test_case "Accept { x -> x } :? int -> int" `Quick test_case_001
    ; test_case "Accept {> self } :? int |@> int" `Quick test_case_002
    ] )
