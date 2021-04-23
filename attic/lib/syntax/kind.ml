module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t) =
struct
  type t = Lambe_ast.Kind.t

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Transept.Utils
  open Lambe_ast.Kind
  open Parser

  let keywords = [ "type"; "->"; "("; ")" ]

  let type_kind = kwd "*" <$> constant Type

  let rec block_kind () = kwd "(" &> do_lazy complex_kind <& kwd ")"

  and simple_kind () = type_kind <|> do_lazy block_kind

  and complex_kind () =
    do_lazy simple_kind
    <&> opt (kwd "->" &> do_lazy complex_kind)
    <$> function
    | k1, None -> k1 | k1, Some k2 -> Lambe_ast.Kind.Arrow (k1, k2)

  let main = complex_kind ()
end
