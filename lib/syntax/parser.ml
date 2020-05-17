module Make
    (CharParser : module type of Transept.Extension.Parser.For_char_list) =
struct
  module CharStream = Transept.Stream.Via_parser (CharParser)

  module Parser =
    Transept.Core.Parser.Make_via_stream
      (CharStream)
      (struct
        type t = Lambe_lexer.Lexeme.t
      end)

  module Kind = Kind.Make_via_parser (Parser)
  module Type = Type.Make_via_parser (Parser) (Kind)
  module Term = Term.Make_via_parser (Parser) (Kind) (Type)
  module Entity = Entity.Make_via_parser (Parser) (Kind) (Type) (Term)

  let stream s =
    let keywords =
      Kind.keywords @ Type.keywords @ Term.keywords @ Entity.keywords
    in
    let module Lexer = Lambe_lexer.Lexer.Make (CharParser) in
    let tokenizer = Lexer.tokenizer keywords in
    CharStream.build tokenizer
      (CharParser.Stream.build @@ Transept.Utils.chars_of_string s)

  include Parser
end
