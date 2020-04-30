module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Transept.Genlex.Lexeme.t) : sig
  val keywords : string list

  val main : Lambe_ast.Kind.t Parser.t
end
