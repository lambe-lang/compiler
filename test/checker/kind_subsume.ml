open Lambe_ast.Kind
open Lambe_checker.Kind

let should_accept_type_and_type () =
  let expected = true
  and computed = Checker.subsume (Type ()) (Type ()) in
  Alcotest.(check bool) "should accept * < *" expected computed

let should_accept_type_to_type_and_type () =
  let expected = true
  and computed = Checker.subsume (Arrow (Type (), Type (), ())) (Type ()) in
  Alcotest.(check bool) "should accept * -> * < *" expected computed

let should_reject_type_and_type_to_type () =
  let expected = false
  and computed = Checker.subsume (Type ()) (Arrow (Type (), Type (), ())) in
  Alcotest.(check bool) "should reject * < * -> *" expected computed

let test_cases =
  let open Alcotest in
  ( "Kind subsume"
  , [
      test_case "Should accept * < *" `Quick should_accept_type_and_type
    ; test_case "Should accept * -> * < *" `Quick
        should_accept_type_to_type_and_type
    ; test_case "Should reject * < * -> *" `Quick
        should_reject_type_and_type_to_type
    ] )
