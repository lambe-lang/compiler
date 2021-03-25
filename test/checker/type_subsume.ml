open Lambe_checker.Context
open Lambe_checker.Gamma
open Lambe_checker.Type.Checker.Operator
open Dsl.Types
module K = Dsl.Kinds

let test_case_000 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty) |- (v "a" <? v "a") Variables.create
  in
  Alcotest.(check bool) "should accept a <? a" expected computed

let test_case_001 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? v "b") Variables.create
  in
  Alcotest.(check bool) "should accept a <? b" expected computed

let test_case_002 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? (v "a" <|> v "b")) Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_003 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <? (v "b" <|> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a <? b | a" expected computed

let test_case_004 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" |-> v "a" <? (v "a" |-> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b -> a <? a -> a" expected computed

let test_case_005 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" |=> v "a" <? (v "a" |=> v "a")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b @-> a <? a @-> a" expected computed

let test_case_006 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "b" <|> v "a" <? (v "a" <|> v "b")) Variables.create
  in
  Alcotest.(check bool) "should accept a | b <? b | a" expected computed

let test_case_007 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- ( data "C" [ "h", v "a"; "t", v "b" ]
       <? data "C" [ "t", v "b"; "h", v "a" ] )
         Variables.create
  in
  Alcotest.(check bool)
    "should accept data C (h:a) (t:b) <? data C (t:b) (h:a)" expected computed

let test_case_008 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (data "C" [ "h", v "a"; "t", v "b" ] <? data "C" [ "t", v "b" ])
         Variables.create
  in
  Alcotest.(check bool)
    "should accept data C (h:a) (t:b) <? data C (t:b)" expected computed

let test_case_009 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (data "C" [ "t", v "b" ] <? data "C" [ "h", v "a"; "t", v "b" ])
         Variables.create
  in
  Alcotest.(check bool)
    "should reject data C (t:b) <? data C (h:a) (t:b)" expected computed

let test_case_010 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (mu "x" (v "a" |-> v "x") <? (v "a" |-> mu "x" (v "a" |-> v "x")))
         Variables.create
  in
  Alcotest.(check bool)
    "should accept mu(x).a -> x <? a -> (mu(x).a -> x)" expected computed

let test_case_011 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" |-> mu "x" (v "a" |-> v "x") <? mu "x" (v "a" |-> v "x"))
         Variables.create
  in
  Alcotest.(check bool)
    "should accept a -> (mu(x).a -> x) <? mu(x).a -> x" expected computed

let test_case_012 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (mu "y" (v "a" |-> v "y") <? mu "x" (v "a" |-> v "x")) Variables.create
  in
  Alcotest.(check bool)
    "should accept mu(y).a -> x <? mu(x).a -> x" expected computed

let test_case_013 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( forall ("x", K.star) (v "x" |-> v "a")
       <? forall ("y", K.star) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should accept forall(x:*).x -> a <? forall(y:*).y -> a" expected computed

let test_case_014 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( forall ("x", K.(star |-> star)) (v "x" |-> v "a")
       <? forall ("y", K.star) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should reject forall(x:*->*).x -> a <? forall(y:*).y -> a" expected
    computed

let test_case_015 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( forall ("x", K.star) (v "x" |-> v "a")
       <? forall ("y", K.(star |-> star)) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should reject forall(x:*).x -> a <? forall(y:*->*).y -> a" expected
    computed

let test_case_016 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( exists ("x", K.star) (v "x" |-> v "a")
       <? exists ("y", K.star) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should accept exists(x:*).x -> a <? exists(y:*).y -> a" expected computed

let test_case_017 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( exists ("x", K.star) (v "x" |-> v "a")
       <? exists ("y", K.(star |-> star)) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should reject exists(x:*).x -> a <? exists(y:*->*).y -> a" expected
    computed

let test_case_018 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( exists ("x", K.(star |-> star)) (v "x" |-> v "a")
       <? exists ("y", K.star) (v "y" |-> v "a") )
         Variables.create
  in
  Alcotest.(check bool)
    "should reject exists(x:*->*).x -> a <? exists(y:*).y -> a" expected
    computed

let test_case_019 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (forall ("x", K.star) (v "x") <$> v "a" <? v "a") Variables.create
  in
  Alcotest.(check bool) "should reject (forall(x:*).x) a <? a" expected computed

let test_case_020 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" <? (forall ("x", K.star) (v "x") <$> v "a")) Variables.create
  in
  Alcotest.(check bool) "should reject a <? (forall(x:*).x) a" expected computed

let test_case_021 () =
  let expected = true
  and computed, _ =
    K.(
      Helpers.k_set [ "a", star ]
      + Helpers.t_set [ "b", lambda ("x", K.star) (v "x") ])
    |- (v "b" <$> v "a" <? v "a") Variables.create
  in
  Alcotest.(check bool)
    "should accept b=(lambda(x:*).x) a <? a" expected computed

let test_case_022 () =
  let expected = true
  and computed, _ =
    K.(
      Helpers.k_set [ "a", star ]
      + Helpers.t_set [ "b", lambda ("x", K.star) (v "x") ])
    |- (v "a" <? (v "b" <$> v "a")) Variables.create
  in
  Alcotest.(check bool)
    "should accept a <? b=(lambda(x:*).x) a" expected computed

let test_case_023 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (v "a" <? trait [] [ "n", v "a" ] [] [] @ v "n") Variables.create
  in
  Alcotest.(check bool)
    "should accept a <? trait { type n = a }.n" expected computed

let test_case_024 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- (trait [] [ "n", v "a" ] [] [] @ v "n" <? v "a") Variables.create
  in
  Alcotest.(check bool)
    "should accept trait { type n = a }.n <? a" expected computed

let test_case_025 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "a", K.star ]
    + Helpers.t_set [ "b", trait [] [ "n", v "a" ] [] [] ]
    |- (v "a" <? v "b" @ v "n") Variables.create
  in
  Alcotest.(check bool)
    "should accept b=trait { type n = a } |- a <? b.n" expected computed

let test_case_026 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "a", K.star ]
    + Helpers.t_set [ "b", trait [] [ "n", v "a" ] [] [] ]
    |- (v "b" @ v "n" <? v "a") Variables.create
  in
  Alcotest.(check bool)
    "should accept b=trait { type n = a } |- b.n <? a" expected computed

let test_case_027 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "a", K.star ]
    + Helpers.t_set [ "b", trait [] [] [] [ gamma [] [ "n", v "a" ] [] [] ] ]
    |- (v "b" @ v "n" <? v "a") Variables.create
  in
  Alcotest.(check bool)
    "should accept b=trait { type n = a } |- b.n <? a" expected computed

let test_case_028 () =
  let expected = true
  and computed, _ =
    Helpers.k_set [ "a", K.star ]
    + Helpers.t_set [ "b", trait [] [] [] [ gamma [] [ "n", v "a" ] [] [] ] ]
    |- (v "a" <? v "b" @ v "n") Variables.create
  in
  Alcotest.(check bool)
    "should accept b=trait { type n = a } |- a <? b.n" expected computed

let test_case_029 () =
  let expected = false
  and computed, _ =
    Helpers.k_set [ "a", K.star ]
    + Helpers.t_set [ "b", trait [] [] [] [ gamma [] [ "m", v "a" ] [] [] ] ]
    |- (v "a" <? v "b" @ v "n") Variables.create
  in
  Alcotest.(check bool)
    "should reject b=trait { type m = a } |- a <? b.n" expected computed

let test_case_030 () =
  let expected = true
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( forall ("x", K.(trait [ "n", star ])) (v "x")
       <? forall ("y", K.(trait [ "n", star; "m", star ])) (v "y") )
         Variables.create
  in
  Alcotest.(check bool)
    "should accept forall(x:{n:a}).x <? forall(y:{n:a,m:a).y" expected computed

let test_case_031 () =
  let expected = false
  and computed, _ =
    K.(Helpers.k_set [ "a", star ] + empty)
    |- ( forall ("x", K.(trait [ "n", star; "m", star ])) (v "x")
       <? forall ("y", K.(trait [ "n", star ])) (v "y") )
         Variables.create
  in
  Alcotest.(check bool)
    "should reject forall(x:{n:a,m:a}).x <? forall(y:{n:a).y" expected computed

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
    ; test_case "Accept mu(x).a -> x <? a -> mu(x).a -> x" `Quick test_case_010
    ; test_case "Accept a -> (mu(x).a -> x) <? mu(x).a -> x" `Quick
        test_case_011
    ; test_case "Accept mu(y).a -> y <? mu(x).a -> x" `Quick test_case_012
    ; test_case "Accept forall(x:*).x -> a <? forall(y:*).y -> a" `Quick
        test_case_013
    ; test_case "Reject forall(x:*->*).x -> a <? forall(y:*).y -> a" `Quick
        test_case_014
    ; test_case "Reject forall(x:*).x -> a <? forall(y:*->*).y -> a" `Quick
        test_case_015
    ; test_case "Accept exists(x:*).x -> a <? exists(y:*).y -> a" `Quick
        test_case_016
    ; test_case "Reject exists(x:*).x -> a <? exists(y:*->*).y -> a" `Quick
        test_case_017
    ; test_case "Reject exists(x:*->*).x -> a <? exists(y:*).y -> a" `Quick
        test_case_018
    ; test_case "Reject (forall(x:*).x) a <? a" `Quick test_case_019
    ; test_case "Reject a <? (forall(x:*).x) a" `Quick test_case_020
    ; test_case "Accept b=(lambda(x:*).x) |- b a <? a" `Quick test_case_021
    ; test_case "Accept b=(lamda(x:*).x) |- a <? b a" `Quick test_case_022
      (*
    ; test_case "Accept a <? trait { type n = a }.n" `Quick test_case_023
    ; test_case "Accept trait { type n = a }.n <? a" `Quick test_case_024
    ; test_case "Accept b=trait { type n = a } |- a <? b.n" `Quick test_case_025
    ; test_case "Accept b=trait { type n = a } |- b.n <? a" `Quick test_case_026
    ; test_case "Accept b=trait with { type n = a } |- b.n <? a" `Quick
        test_case_027
    ; test_case "Accept b=trait with { type n = a } |- a <? b.n" `Quick
        test_case_028
    *)
    ; test_case "Reject b=trait with { type m = a } |- a <? b.n" `Quick
        test_case_029
    ; test_case "Accept forall(x:{n:*}).x <? forall(y:{n:*,m:*).y" `Quick
        test_case_030
    ; test_case "Reject forall(x:{n:*,m=*}).x <? forall(y:{n:*).y" `Quick
        test_case_031
    ] )
