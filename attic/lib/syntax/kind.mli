module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t) :
  Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t
