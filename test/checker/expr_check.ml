open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Expr.Checker.Operator
open Dsl.Exprs
module T = Dsl.Types
module K = Dsl.Kinds
module H = Helpers

let test_case_000 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ "a", T.v "int" ]
    |- (v "a" <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool) "should accept a :? int" expected computed

let test_case_001 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    |- (lambda "x" (v "x") <:?> T.(v "int" |-> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept { x -> x } :? int -> int" expected computed

let test_case_002 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    |- (zeta (v "self") <:?> T.(v "int" |=> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept {> self } :? int |@> int" expected computed

let test_case_003 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int" ])) Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) (w:int) |- r :? data d (v:int)" expected
    computed

let test_case_004 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(data "r" [ "v", v "int"; "w", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int" ])) Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) (w:int) |- r :? data d (v:int)" expected
    computed

let test_case_005 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int"; "w", v "int" ]))
         Variables.create
  in
  Alcotest.(check bool)
    "should reject r:data d (v:int) |- r :? data d (v:int) (w:int)" expected
    computed

let test_cases =
  let open Alcotest in
  ( "Expression check"
  , [
      test_case "Accept a :? int" `Quick test_case_000
    ; test_case "Accept { x -> x } :? int -> int" `Quick test_case_001
    ; test_case "Accept {> self } :? int |@> int" `Quick test_case_002
    ; test_case "Accept r:data d (v:int) |- r :? data d (v:int)" `Quick
        test_case_003
    ; test_case "Accept r:data d (v:int) (w:int) |- r :? data d (v:int)" `Quick
        test_case_004
    ; test_case "Reject r:data d (v:int) |- r :? data d (v:int) (w:int)" `Quick
        test_case_005
    ] )
