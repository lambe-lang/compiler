module Make (CharParser : module type of Transept.Extension.Parser.For_char_list) = struct
  module CharStream = Transept.Stream.Via_parser (CharParser)

  module Parser =
    Transept.Core.Parser.Make_via_stream
      (CharStream)
      (struct
        type t = Lexer.Lexeme.t
      end)

  module Kind = Kind.Make_via_parser (Parser)
  module Type = Type.Make_via_parser (Parser)
  module Term = Term.Make_via_parser (Parser)

  let stream s =
    let keywords = Kind.keywords @ Type.keywords @ Term.keywords in
    let module Lexer = Lexer.Make (CharParser) in
    let tokenizer = Lexer.tokenizer_with_spaces keywords in
    CharStream.build tokenizer (CharParser.Stream.build @@ Transept.Utils.chars_of_string s)

  include Parser
end
