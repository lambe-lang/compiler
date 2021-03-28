open Lambe_checker.Gamma
open Lambe_checker.Type.Checker.Operator
open Dsl.Types
module K = Dsl.Kinds

let test_case_0000 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", star ] + empty) |- (v "a" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a :? *" expected computed

let test_case_000_a () =
  let expected = true
  and computed =
    K.(Helpers.w_set [ Helpers.k_set [ "a", star ] ]) |- (v "a" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a :? *" expected computed

let test_case_0010 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", star ] + empty) |- (v "a" |-> v "a" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a -> a :? *" expected computed

let test_case_0020 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", star ] + empty) |- (v "a" |=> v "a" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a @> a :? *" expected computed

let test_case_0030 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", star |-> star; "b", star ] + empty)
    |- (v "a" <$> v "b" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a b :? *" expected computed

let test_case_0040 () =
  let expected = false
  and computed =
    K.(Helpers.k_set [ "a", star ] + empty) |- (v "a" <$> v "a" <:?> K.star)
  in
  Alcotest.(check bool) "should reject a:* |- a a :? *" expected computed

let test_case_0050 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", trait [ "n", star ] ] + empty)
    |- (v "a" @ v "n" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a.n :? {n:*}" expected computed

let test_case_0060 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "a", star; "b", star ] + empty)
    |- (v "a" <|> v "b" <:?> K.star)
  in
  Alcotest.(check bool) "should accept a | b :? *" expected computed

let test_case_0070 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (lambda ("x", K.star) (v "b") <:?> K.(star |-> star))
  in
  Alcotest.(check bool) "should accept lambda(x:*).b :? *->*" expected computed

let test_case_0080 () =
  let expected = false
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (lambda ("x", K.star) (v "b") <:?> K.star)
  in
  Alcotest.(check bool) "should reject lambda(x:*).b :? *" expected computed

let test_case_0090 () =
  let expected = false
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (forall ("x", K.star) (v "b") <:?> K.(star |-> star))
  in
  Alcotest.(check bool) "should reject forall(x:*).b :? *->*" expected computed

let test_case_0100 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (forall ("x", K.star) (v "b") <:?> K.star)
  in
  Alcotest.(check bool) "should accept forall(x:*).b :? *" expected computed

let test_case_0011 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (exists ("x", K.star) (v "b") <:?> K.star)
  in
  Alcotest.(check bool) "should accept exists(x:*).b :? *->*" expected computed

let test_case_0120 () =
  let expected = false
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (exists ("x", K.star) (v "b") <:?> K.(star |-> star))
  in
  Alcotest.(check bool) "should accept exists(x:*).b :? *->*" expected computed

let test_case_0130 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star ] + empty)
    |- (mu ("x", K.star) (v "x") <:?> K.star)
  in
  Alcotest.(check bool) "should accept mu(x:*).x :? *" expected computed

let test_case_0140 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star |-> star ] + empty)
    |- (mu ("x", K.star) (v "b" <$> v "x") <:?> K.star)
  in
  Alcotest.(check bool)
    "should accept b:*->* |- mu(x:*).(b x) :? *" expected computed

let test_case_0150 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", trait [ "n", star ] ]) |- (v "b" @ v "n" <:?> K.star)
  in
  Alcotest.(check bool)
    "should accept b=trait { n:* } |- b.n:*" expected computed

let test_case_0160 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", trait [ "n", star; "m", star ] ])
    |- (v "b" @ v "n" <:?> K.star)
  in
  Alcotest.(check bool)
    "should accept b=trait { n:*, m:* } |- b.n:*" expected computed

let test_case_0170 () =
  let expected = false
  and computed =
    K.(Helpers.k_set [ "b", trait [ "m", star ] ]) |- (v "b" @ v "n" <:?> K.star)
  in
  Alcotest.(check bool)
    "should reject b=trait { m:* } |- b.n:*" expected computed

let test_case_0180 () =
  let expected = true
  and computed =
    K.(Helpers.k_set [ "b", star ]) |- (data "r" [ "x", v "b" ] <:?> K.star)
  in
  Alcotest.(check bool) "should accept data r (x:b) : *" expected computed

let test_cases =
  let open Alcotest in
  ( "Type check"
  , [
      test_case "Accept a :? *" `Quick test_case_0000
    ; test_case "Accept a :? *" `Quick test_case_000_a
    ; test_case "Accept a -> a :? *" `Quick test_case_0010
    ; test_case "Accept a @> a :? *" `Quick test_case_0020
    ; test_case "Accept a b :? *" `Quick test_case_0030
    ; test_case "Reject a:* |- a a :? *" `Quick test_case_0040
    ; test_case "Accept a.n :? {n:*}" `Quick test_case_0050
    ; test_case "Accept a | b :? *" `Quick test_case_0060
    ; test_case "Accept lambda(x:*).b :? *->*" `Quick test_case_0070
    ; test_case "Reject lambda(x:*).b :? *" `Quick test_case_0080
    ; test_case "Reject forall(x:*).b :? *->*" `Quick test_case_0090
    ; test_case "Accept forall(x:*).b :? *" `Quick test_case_0100
    ; test_case "Accept exists(x:*).b :? *" `Quick test_case_0011
    ; test_case "Reject exists(x:*).b :? *->*" `Quick test_case_0120
    ; test_case "Accept mu(x:*).x :? *" `Quick test_case_0130
    ; test_case "Accept b:*->* |- mu(x:*).(b x) :? *" `Quick test_case_0140
    ; test_case "Accept b=trait { n : * } |- b.n:*" `Quick test_case_0150
    ; test_case "Accept b=trait { n : *, m:* } |- b.n:*" `Quick test_case_0160
    ; test_case "Reject b=trait { m:* } |- b.n:*" `Quick test_case_0170
    ; test_case "Accept data r (x:b) : *" `Quick test_case_0180
    ] )
