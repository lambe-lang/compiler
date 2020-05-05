let lambe_term = Alcotest.testable Lambe.Render.Term.pp ( = )

let transept_lexeme = Alcotest.testable Lambe.Syntax.Lexer.Lexeme.pp ( = )

let should_parse input expected =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let expected = Ok expected
  and computed =
    Response.fold
      (parse Term.main @@ stream input)
      (fun (_, a, _) -> Ok a)
      (fun (s, _) -> Error (Stream.position s))
  in
  Alcotest.(check (result lambe_term int)) "should_parse" expected computed

let cases =
  let open Lambe.Ast.Term in
  [
    "123.45", Native (Float 123.45)
  ; "\"Hello\"", Native (String "Hello")
  ; "'c'", Native (Char 'c')
  ; "a", Variable "a"
  ; "()", Variable "()"
  ; "{ _ }", Abstraction ("_", Variable "_")
  ; "{ a -> a }", Abstraction ("a", Variable "a")
  ; "{ a b -> a }", Abstraction ("a", Abstraction ("b", Variable "a"))
  ; "a ()", Apply (Variable "a", Variable "()")
  ; "a b c", Apply (Apply (Variable "a", Variable "b"), Variable "c")
  ; "a (b c)", Apply (Variable "a", Apply (Variable "b", Variable "c"))
  ; "let a = 23 in 'c'", Let ("a", Native (Float 23.), Native (Char 'c'))
  ; ( "let a b = b in a 23"
    , Let ("a", Abstraction ("b", Variable "b"), Apply (Variable "a", Native (Float 23.))) )
  ; "a.m", Apply (Variable "a", Variable "m")
  ; "a.(::)", Apply (Variable "a", Variable "::")
  ; "a ::", Apply (Variable "a", Variable "::")
  ; ( "when a { is (::) -> a }"
    , When (None, Variable "a", [ Lambe.Ast.Type.Variable "::", Variable "a" ]) )
  ; ( "when a { is (::) -> f true is Nil -> f false }"
    , When
        ( None
        , Variable "a"
        , [
            Lambe.Ast.Type.Variable "::", Apply (Variable "f", Variable "true")
          ; Lambe.Ast.Type.Variable "Nil", Apply (Variable "f", Variable "false")
          ] ) )
  ]

let test_cases =
  let open Alcotest in
  ( "Term Parser"
  , List.map
      (fun (input, expected) ->
        test_case ("Should parse " ^ input) `Quick (fun () -> should_parse input expected))
      cases )
