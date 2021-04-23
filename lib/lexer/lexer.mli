module Make (Parser : Transept_specs.PARSER with type e = char) : sig
  val tokenizer : string list -> Lexeme.t Parser.t
end

module Token (Parser : Transept_specs.PARSER with type e = Lexeme.t) : sig
  val eol : unit Parser.t

  val spaces : char list Parser.t

  val integer : int Parser.t

  val float : float Parser.t

  val string : string Parser.t

  val char : char Parser.t

  val ident : string Parser.t

  val operator : string Parser.t

  val kwd : string -> string Parser.t

  val is_kwd : string -> Lexeme.t Parser.t
end
