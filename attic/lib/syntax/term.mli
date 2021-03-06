module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t)
    (Type : Entry.API with type t = Lambe_ast.Type.t and type 'a p = 'a Parser.t) :
  Entry.API with type t = Lambe_ast.Term.t and type 'a p = 'a Parser.t
