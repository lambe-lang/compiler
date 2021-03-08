let lambe_kind = Alcotest.testable Lambe_render.Kind.pp ( = )

let should_parse input expected =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe_syntax.Parser.Make (CharParser) in
  let expected = Ok expected
  and computed =
    Response.fold
      (parse Kind.main @@ stream input)
      (fun (_, a, _) -> Ok a)
      (fun (s, _) -> Error (Stream.position s))
  in
  Alcotest.(check (result lambe_kind int)) "should_parse" expected computed

let cases =
  let open Lambe_ast.Kind in
  [
    "type", Type
  ; "(type)", Type
  ; "type -> type", Arrow (Type, Type)
  ; "(type) -> type", Arrow (Type, Type)
  ; "type -> (type)", Arrow (Type, Type)
  ; "type -> type -> type", Arrow (Type, Arrow (Type, Type))
  ; "(type -> type) -> type", Arrow (Arrow (Type, Type), Type)
  ]

let test_cases =
  let open Alcotest in
  ( "Kind Parser"
  , List.map
      (fun (input, expected) ->
        test_case ("Should parse " ^ input) `Quick (fun () ->
            should_parse input expected ))
      cases )
