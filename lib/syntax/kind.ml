module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Transept.Genlex.Lexeme.t) =
struct
  open Transept.Genlex.Lexer.Token (Parser)

  open Transept.Utils
  open Parser

  let keywords = [ "type"; "->"; "("; ")"; "["; "]" ]

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

  let main = complex_kind ()
end
