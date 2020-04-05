(** The kind parser is able to recognize simple syntaxic construction give by
    the following grammar:

    {v
kind_simple ::= '(' kind ')' | "type"
kind        ::= kind_simple ("->" kind)?
    v} *)

module Make
    (Parser : Transept_specs.PARSER with type e = Transept.Genlex.Lexeme.t) =
struct
  open Transept.Genlex.Lexer.Token (Parser)

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
