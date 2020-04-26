let lambe_kind = Alcotest.testable Lambe.Render.Kind.pp ( = )

let transept_lexeme = Alcotest.testable Transept.Genlex.PrettyPrinter.pp ( = )

let should_parse_kind () =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let open Lambe.Ast.Kind in
  let expected = Some Type
  and computed =
    Response.fold
      (parse Kind.main @@ lexeme_stream "type")
      (fun (_, a, _) -> Some a)
      (fun _ -> None)
  in
  Alcotest.(check (option lambe_kind)) "should_parse_kind" expected computed

let should_parse_kind_in_parenthesis () =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let open Lambe.Ast.Kind in
  let expected = Some Type
  and computed =
    Response.fold
      (parse Kind.main @@ lexeme_stream "( type )")
      (fun (_, a, _) -> Some a)
      (fun _ -> None)
  in
  Alcotest.(check (option lambe_kind))
    "should_parse_kind_in_parenthesis" expected computed

let should_parse_functional_kind () =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let open Lambe.Ast.Kind in
  let open Parser in
  let expected = Some (Arrow (Type, Type))
  and computed =
    Response.fold
      (parse Kind.main @@ lexeme_stream "type -> type")
      (fun (_, a, _) -> Some a)
      (fun _ -> None)
  in
  Alcotest.(check (option lambe_kind))
    "should_parse_functional_kind" expected computed

let should_parse_complex_functional_kind () =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let open Lambe.Ast.Kind in
  let open Parser in
  let expected = Some (Arrow (Arrow (Type, Type), Type))
  and computed =
    Response.fold
      (parse Kind.main @@ lexeme_stream "(type -> type) -> type")
      (fun (_, a, _) -> Some a)
      (fun _ -> None)
  in
  Alcotest.(check (option lambe_kind))
    "should_parse_complex_functional_kind" expected computed

let should_read_all_tokens () =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let module Parser = Lambe.Syntax.Parser.Make (CharParser) in
  let module Stream = Transept.Stream.Via_parser (CharParser) in
  let module Iterator = Transept.Stream.Iterator (Stream) in
  let to_list stream = Iterator.fold_right (fun e l -> e :: l) stream [] in
  let open Transept.Genlex.Lexeme in
  let expected = [ Keyword "type"; Keyword "("; Keyword "->"; Keyword ")" ]
  and computed = to_list (Parser.lexeme_stream "type ( -> )") in
  Alcotest.(check (list transept_lexeme))
    "should_read_all_tokens" expected computed

let test_cases =
  let open Alcotest in
  ( "Kind Parser"
  , [
      test_case "Should parse kind" `Quick should_parse_kind
    ; test_case "Should parse kind in parenthesis" `Quick
        should_parse_kind_in_parenthesis
    ; test_case "Should parse functional kind" `Quick
        should_parse_functional_kind
    ; test_case "Should parse complexe functional kind" `Quick
        should_parse_complex_functional_kind
    ; test_case "Should read all tokens" `Quick should_read_all_tokens
    ] )
