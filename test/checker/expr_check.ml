open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Expr.Checker.Operator
open Dsl.Exprs
module T = Dsl.Types
module K = Dsl.Kinds
module H = Helpers

let test_case_0000 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ "a", T.v "int" ]
    |- (v "a" <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool) "should accept a :? int" expected computed

let test_case_0001 () =
  let expected = false
  and computed, _ =
    H.s_set [ "a", T.v "int" ] |- (v "a" <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool) "reject accept a :? int" expected computed

let test_case_0010 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    |- (lambda "x" (v "x") <:?> T.(v "int" |-> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept { x -> x } :? int -> int" expected computed

let test_case_0020 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    |- (zeta (v "self") <:?> T.(v "int" |=> v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept {> self } :? int |@> int" expected computed

let test_case_0030 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int" ])) Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) (w:int) |- r :? data d (v:int)" expected
    computed

let test_case_0040 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(data "r" [ "v", v "int"; "w", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int" ])) Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) (w:int) |- r :? data d (v:int)" expected
    computed

let test_case_0050 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (v "r" <:?> T.(data "r" [ "v", T.v "int"; "w", v "int" ]))
         Variables.create
  in
  Alcotest.(check bool)
    "should reject r:data d (v:int) |- r :? data d (v:int) (w:int)" expected
    computed

let test_case_0060 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (use (v "r") (v "v") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) |- r.v :? int" expected computed

let test_case_0070 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star ] + H.s_set [ ("r", T.(data "r" [ "v", v "int" ])) ]
    |- (use (v "r") (v "w") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data d (v:int) |- r.w :? int" expected computed

let test_case_0080 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(trait [] [] [ "v", v "int" ] [])) ]
    |- (use (v "r") (v "v") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:trait { sig v:int } |- r.v :? int" expected computed

let test_case_0090 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(trait [] [] [ "v", v "int" ] [])) ]
    |- (use (v "r") (v "w") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should reject r:trait { sig v:int } |- r.w :? int" expected computed

let test_case_0100 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(trait [] [] [ "v", data "r" [ "w", v "int" ] ] [])) ]
    |- (use (use (v "r") (v "v")) (v "w") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:trait { sig v:data r (w:int) } |- r.v :? int" expected
    computed

let test_case_0110 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(data "r" [ "v", trait [] [] [ "w", v "int" ] [] ])) ]
    |- (use (use (v "r") (v "v")) (v "w") <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data r (v:trait { sig w:int }) |- (r.v).w :? int" expected
    computed

let test_case_0120 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(data "r" [ "v", trait [] [] [ "w", v "int" ] [] ])) ]
    |- (use (v "r") (use (v "v") (v "w")) <:?> T.v "int") Variables.create
  in
  Alcotest.(check bool)
    "should accept r:data r (v:trait { sig w:int }) |- r.(v.w) :? int" expected
    computed

let test_case_0130 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(lambda ("x", K.star) (v "x") <$> v "int")) ]
    |- (v "r" <:?> T.(v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept r:(lambda(x:*).x) int |- r :? int" expected computed

let test_case_0140 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(v "int" |-> v "int")); "a", T.v "int" ]
    |- (v "r" <$> v "a" <:?> T.(v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept r: int -> int, a : int |- r a :? int" expected computed

let test_case_0150 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star; "string", K.star ]
    + H.s_set [ ("r", T.(v "string" |-> v "int")); "a", T.v "int" ]
    |- (v "r" <$> v "a" <:?> T.(v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should reject r: string -> int, a : int |- r a :? int" expected computed

let test_case_0160 () =
  let expected = true
  and computed, _ =
    H.k_set [ "int", K.star ]
    + H.s_set [ ("r", T.(v "int" |=> v "int")); "a", T.v "int" ]
    |- (v "a" <$> v "r" <:?> T.(v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should accept r: int |@> int, a : int |- a r :? int" expected computed

let test_case_0170 () =
  let expected = false
  and computed, _ =
    H.k_set [ "int", K.star; "string", K.star ]
    + H.s_set [ ("r", T.(v "string" |=> v "int")); "a", T.v "int" ]
    |- (v "a" <$> v "r" <:?> T.(v "int")) Variables.create
  in
  Alcotest.(check bool)
    "should reject r: string => int, a : int |- a r :? int" expected computed

let test_cases =
  let open Alcotest in
  ( "Expression check"
  , [
      test_case "Accept int:* |- a :? int" `Quick test_case_0000
    ; test_case "Reject a :? int" `Quick test_case_0001
    ; test_case "Accept { x -> x } :? int -> int" `Quick test_case_0010
    ; test_case "Accept @{ self } :? int |@> int" `Quick test_case_0020
    ; test_case "Accept r:data d (v:int) |- r :? data d (v:int)" `Quick
        test_case_0030
    ; test_case "Accept r:data d (v:int) (w:int) |- r :? data d (v:int)" `Quick
        test_case_0040
    ; test_case "Reject r:data d (v:int) |- r :? data d (v:int) (w:int)" `Quick
        test_case_0050
    ; test_case "Accept r:data d (v:int) |- r.v :? int" `Quick test_case_0060
    ; test_case "Reject r:data d (v:int) |- r.w :? int" `Quick test_case_0070
    ; test_case "Accept r:trait { sig v:int } |- r.v :? int" `Quick
        test_case_0080
    ; test_case "Reject r:trait { sig v:int } |- r.w :? int" `Quick
        test_case_0090
    ; test_case "Accept r:trait { sig v:data r (w:int) ] } |- r.v.w :? int"
        `Quick test_case_0100
    ; test_case "Accept r:data r (v:trait { sig w:int }) |- (r.v).w :? int"
        `Quick test_case_0110
    ; test_case "Accept r:data r (v:trait { sig w:int }) |- r.(v.w) :? int"
        `Quick test_case_0120
    ; test_case "Accept r:(lambda(x:*).x) int |- r :? int" `Quick test_case_0130
    ; test_case "Accept r: int -> int, a : int |- r a :? int" `Quick
        test_case_0140
    ; test_case "Reject r: string -> int, a : int |- r a :? int" `Quick
        test_case_0150
    ; test_case "Accept r: int => int, a : int |- a r :? int" `Quick
        test_case_0160
    ; test_case "Reject r: string => int, a : int |- a r :? int" `Quick
        test_case_0170
    ] )
