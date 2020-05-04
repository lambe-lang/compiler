module Make_via_parser (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t) : sig
  val keywords : string list

  val main : Lambe_ast.Type.t Parser.t
end
