module Lexeme : sig
  type t =
    | Float of float
    | String of string
    | Char of char
    | Ident of string
    | Operator of string
    | Keyword of string

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Make (Parser : Transept_specs.PARSER with type e = char) : sig
  val tokenizer_with_spaces : string list -> Lexeme.t Parser.t
end

module Token (Parser : Transept_specs.PARSER with type e = Lexeme.t) : sig
  val float : float Parser.t

  val string : string Parser.t

  val char : char Parser.t

  val operator : string Parser.t

  val ident : string Parser.t

  val kwd : string -> string Parser.t
end
