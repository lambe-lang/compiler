module Make_via_parser (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t) = struct
  open Lexer.Token (Parser)

  open Parser

  (* open Lambe_ast.Entities *)

  let keywords = [ "sig"; "def"; "trait"; "type"; "data"; ":" ]

  (*
  *)

  let main = fail
end
