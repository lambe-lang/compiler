let lambe_entity = Alcotest.testable Lambe.Render.Entity.pp ( = )

let should_parse input expected =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let expected = Ok expected
  and computed =
    Response.fold
      (parse Entity.main @@ stream input)
      (fun (_, a, _) -> Ok a)
      (fun (s, _) -> Error (Stream.position s))
  in
  Alcotest.(check (result lambe_entity int)) "should_parse" expected computed

let cases =
  let open Lambe.Ast in
  [
    ( "kind (->) = type -> type -> type"
    , Entity.Kind ("->", Kind.(Arrow (Type, Arrow (Type, Type)))) )
  ; ( "sig (::) : forall a. list a"
    , Entity.Sig
        ( "::"
        , Type.(Forall ("a", Kind.Type, Apply (Variable "list", Variable "a")))
        , None
        , [] ) )
  ; ( "sig (::) : a for a"
    , Entity.Sig ("::", Type.Variable "a", Some (Type.Variable "a"), []) )
  ; ( "sig (::) : a with a"
    , Entity.Sig ("::", Type.Variable "a", None, [ Type.Variable "a" ]) )
  ; ( "sig (::) : a for a with a"
    , Entity.Sig
        ( "::"
        , Type.Variable "a"
        , Some (Type.Variable "a")
        , [ Type.Variable "a" ] ) )
  ; ( "sig (::) : a for a with a with b"
    , Entity.Sig
        ( "::"
        , Type.Variable "a"
        , Some (Type.Variable "a")
        , [ Type.Variable "a"; Type.Variable "b" ] ) )
  ; "sig (()) : unit", Entity.Sig ("()", Type.Variable "unit", None, [])
  ; ( "sig (++) : forall a. self -> self for int"
    , Entity.Sig
        ( "++"
        , (let open Type in
          Forall
            ( "a"
            , Kind.Type
            , Apply (Apply (Variable "->", Variable "self"), Variable "self") ))
        , Some (Type.Variable "int")
        , [] ) )
  ; ( "sig (=) : forall a. self -> self -> bool for a with Eq a"
    , Entity.Sig
        ( "="
        , (let open Type in
          Forall
            ( "a"
            , Kind.Type
            , Apply
                ( Apply (Variable "->", Variable "self")
                , Apply (Apply (Variable "->", Variable "self"), Variable "bool")
                ) ))
        , Some (Type.Variable "a")
        , [ Type.(Apply (Variable "Eq", Variable "a")) ] ) )
  ; ( "def (++) a b = a + b"
    , Entity.Def
        ( "++"
        , let open Term in
          Abstraction
            ( "a"
            , Abstraction
                ("b", Apply (Apply (Variable "a", Variable "+"), Variable "b"))
            ) ) )
  ; "def (()) = unit", Entity.Def ("()", Term.Variable "unit")
  ; "data Zero", Entity.Data ("Zero", [], [])
  ; ( "data Succ { p : Peano } "
    , Entity.Data ("Succ", [], [ "p", Type.Variable "Peano" ]) )
  ; ( "data List a { h : a; t : List a } "
    , Entity.Data
        ( "List"
        , [ "a", Kind.Type ]
        , [
            "h", Type.Variable "a"
          ; ("t", Type.(Apply (Variable "List", Variable "a")))
          ] ) )
  ; ( "type Peano = Zero | Succ"
    , Entity.Enum ("Peano", [], [ Type.Variable "Zero"; Type.Variable "Succ" ])
    )
  ; ( "type fun a b = a -> b"
    , Entity.Type
        ( "fun"
        , [ "a", Kind.Type; "b", Kind.Type ]
        , Apply (Apply (Variable "->", Variable "a"), Variable "b") ) )
  ; "trait Combinable", Entity.Trait ("Combinable", [], None, [], [])
  ; ( "trait Combinable { sig (+) : self -> self }"
    , Entity.Trait
        ( "Combinable"
        , []
        , None
        , []
        , [
            Entity.Sig
              ( "+"
              , (let open Type in
                Apply (Apply (Variable "->", Variable "self"), Variable "self"))
              , None
              , [] )
          ] ) )
  ; ( "trait Combinable a for a"
    , Entity.Trait
        ("Combinable", [ "a", Kind.Type ], Some (Variable "a"), [], []) )
  ]

let test_cases =
  let open Alcotest in
  ( "Entity Parser"
  , List.map
      (fun (input, expected) ->
        test_case ("Should parse " ^ input) `Quick (fun () ->
            should_parse input expected))
      cases )
