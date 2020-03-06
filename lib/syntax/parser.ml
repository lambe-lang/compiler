module Make
    (CharParser : module type of Transept.Extension.Parser.For_char_list) =
struct
  let keywords = [ "type"; "->"; "("; ")"; "["; "]" ]

  module CharStream = Transept.Stream.Via_parser (CharParser)

  module Parser =
    Transept.Core.Parser.Make_via_stream
      (CharStream)
      (struct
        type t = Transept.Genlex.Lexeme.t
      end)

  module Kind = Kind.Make (Parser)

  let lexeme_stream s =
    let module Genlex = Transept_genlex.Lexer.Make (CharParser) in
    let tokenizer = Genlex.tokenizer_with_spaces keywords in
    CharStream.build tokenizer
      (CharParser.Stream.build @@ Transept.Utils.chars_of_string s)

  include Parser
end
