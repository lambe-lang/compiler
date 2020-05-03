let lambe_type = Alcotest.testable Lambe.Render.Type.pp ( = )

let transept_lexeme = Alcotest.testable Lambe.Syntax.Lexer.Lexeme.pp ( = )

let should_parse input expected =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let expected = Some expected
  and computed =
    Response.fold
      (parse Type.main @@ stream input)
      (fun (_, a, _) -> Some a)
      (fun _ -> None)
  in
  Alcotest.(check (option lambe_type)) "should_parse" expected computed

let cases =
  let open Lambe.Ast.Type in
  [
    "self", Variable "self"
  ; "a", Variable "a"
  ; "(->)", Ident "->"
  ; "(->) a", Apply (Ident "->", Variable "a")
  ; "(->) a b", Apply (Apply (Ident "->", Variable "a"), Variable "b")
  ; "(~>) a b", Apply (Apply (Ident "~>", Variable "a"), Variable "b")
  ; "a -> b", Apply (Apply (Ident "->", Variable "a"), Variable "b")
  ; "a ~> b", Apply (Apply (Ident "~>", Variable "a"), Variable "b")
  ; "((->) a) ((::) b)", Apply (Apply (Ident "->", Variable "a"), Apply (Ident "::", Variable "b"))
  ; "map a b", Apply (Apply (Variable "map", Variable "a"), Variable "b")
  ]

let test_cases =
  let open Alcotest in
  ( "Type Parser"
  , List.map
      (fun (input, expected) ->
        test_case ("Should parse " ^ input) `Quick (fun () ->
            should_parse input expected))
      cases )
