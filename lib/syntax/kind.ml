module Lexeme = Transept.Genlex.Lexeme
module Genlex = Transept.Genlex.Lexer

module Make (Parser : Transept_specs.PARSER with type e = Lexeme.t) = struct
  open Genlex.Token (Parser)

  open Transept.Utils
  open Parser

  let rec simple_kind () =
    kwd "("
    &> do_lazy complex_kind
    <& kwd ")"
    <|> (kwd "type" <$> constant Lambe_ast.Kind.Type)

  and complex_kind () =
    do_lazy simple_kind
    <&> opt (kwd "->" &> do_lazy complex_kind)
    <$> function
    | k1, None -> k1 | k1, Some k2 -> Lambe_ast.Kind.Arrow (k1, k2)

  let kind = complex_kind ()
end
