let lambe_type = Alcotest.testable Lambe_render.Type.pp ( = )

let should_parse input expected =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe_syntax.Parser.Make (CharParser) in
  let expected = Ok expected
  and computed =
    Response.fold
      (parse Type.main @@ stream input)
      (fun (_, a, _) -> Ok a)
      (fun (s, _) -> Error (Stream.position s))
  in
  Alcotest.(check (result lambe_type int)) "should_parse" expected computed

let cases =
  let open Lambe_ast.Type in
  [
    "self", Variable "self"
  ; "a", Variable "a"
  ; "(->)", Variable "->"
  ; "(->) a", Apply (Variable "->", Variable "a")
  ; "(->) a b", Apply (Apply (Variable "->", Variable "a"), Variable "b")
  ; "(~>) a b", Apply (Apply (Variable "~>", Variable "a"), Variable "b")
  ; "a -> b", Apply (Apply (Variable "->", Variable "a"), Variable "b")
  ; "a * b", Apply (Apply (Variable "*", Variable "a"), Variable "b")
  ; ( "list a || b"
    , Apply
        ( Apply (Variable "||", Apply (Variable "list", Variable "a"))
        , Variable "b" ) )
  ; ( "a || list b"
    , Apply
        ( Apply (Variable "||", Variable "a")
        , Apply (Variable "list", Variable "b") ) )
  ; ( "((->) a) ((::) b)"
    , Apply
        ( Apply (Variable "->", Variable "a")
        , Apply (Variable "::", Variable "b") ) )
  ; "map a b", Apply (Apply (Variable "map", Variable "a"), Variable "b")
  ; ( "forall a b.a"
    , Forall
        ( "a"
        , Lambe_ast.Kind.Type
        , Forall ("b", Lambe_ast.Kind.Type, Variable "a") ) )
  ; ( "forall (a:type->type) .a"
    , Forall
        ( "a"
        , Lambe_ast.Kind.Arrow (Lambe_ast.Kind.Type, Lambe_ast.Kind.Type)
        , Variable "a" ) )
  ]

let test_cases =
  let open Alcotest in
  ( "Type Parser"
  , List.map
      (fun (input, expected) ->
        test_case ("Should parse " ^ input) `Quick (fun () ->
            should_parse input expected ))
      cases )
