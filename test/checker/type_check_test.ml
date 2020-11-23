open Lambe_ast
open Lambe_checker

let lambe_type = Alcotest.testable Lambe_render.Type.pp ( = )

let should_synthetize_int_type () =
  let expected = Result.Ok (Type.Variable "int")
  and computed = Type_check.synthetize () Term.(Literal (Integer 1)) in
  Alcotest.(check (result lambe_type string))
    "should_synthetize_int_type" expected computed

let should_synthetize_float_type () =
  let expected = Result.Ok (Type.Variable "float")
  and computed = Type_check.synthetize () Term.(Literal (Float 1.)) in
  Alcotest.(check (result lambe_type string))
    "should_synthetize_float_type" expected computed

let should_synthetize_string_type () =
  let expected = Result.Ok (Type.Variable "string")
  and computed = Type_check.synthetize () Term.(Literal (String "1")) in
  Alcotest.(check (result lambe_type string))
    "should_synthetize_string_type" expected computed

let should_synthetize_char_type () =
  let expected = Result.Ok (Type.Variable "char")
  and computed = Type_check.synthetize () Term.(Literal (Char '1')) in
  Alcotest.(check (result lambe_type string))
    "should_synthetize_char_type" expected computed

let test_cases =
  let open Alcotest in
  ( "Type_check"
  , [
      test_case "Should synthesize int type" `Quick should_synthetize_int_type
    ; test_case "Should synthesize float type" `Quick
        should_synthetize_float_type
    ; test_case "Should synthesize string type" `Quick
        should_synthetize_string_type
    ; test_case "Should synthesize char type" `Quick should_synthetize_char_type
    ] )
