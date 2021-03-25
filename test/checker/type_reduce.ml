open Lambe_checker.Gamma
open Lambe_checker.Type.Checker.Operator
open Dsl.Types
module K = Dsl.Kinds

let test_case_000 () =
  let expected = true
  and computed =
    Helpers.t_set [ "a", data "r" [] ] |- v "a" --> Some (data "r" [])
  in
  Alcotest.(check bool)
    "should reduce a:data r |- a --> data r" expected computed

let test_case_001 () =
  let expected = true
  and computed =
    Helpers.k_set [ "a", K.star ]
    |- (lambda ("v", K.star) (v "v") <$> v "a") --> Some (v "a")
  in
  Alcotest.(check bool)
    "Accept a:* |- (lambda (v:*).v) a --> a" expected computed

let test_case_002 () =
  let expected = true
  and computed =
    Helpers.t_set [ "a", trait [ "c", K.star ] [ "b", v "c" ] [] [] ]
    |- (v "a" @ v "b")
       --> Some (trait [ "c", K.star ] [ "b", v "c" ] [] [] @ v "c")
  in
  Alcotest.(check bool)
    "Accept a:* |- (lambda (v:*).v) a --> a" expected computed

let test_cases =
  let open Alcotest in
  ( "Type reduce"
  , [
      test_case "Accept a:data r |- a" `Quick test_case_000
    ; test_case "Accept a:* |- (lambda (v:*).v) a --> a" `Quick test_case_001
    ; test_case "Accept a:trait { kind c:* type b=c }  |- a.b --> c" `Quick
        test_case_002
    ] )
