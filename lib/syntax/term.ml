module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t) =
struct
  open Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Term

  let keywords = [ "self"; "("; ")"; "{"; "}"; "let"; "="; "->"; "in" ]

  let native_term =
    float
    <$> (fun f -> Native (Float f))
    <|> (string <$> (fun f -> Native (String f)))
    <|> (char <$> (fun f -> Native (Char f)))

  let ident_term = ident <$> (fun f -> Variable f)

  let rec let_term () =
    kwd "let"
    &> ident
    <& kwd "="
    <&> do_lazy simple_term
    <& kwd "in"
    <&> do_lazy simple_term
    <$> (fun ((i, a), b) -> Let (i, a, b))

  and function_term () =
    kwd "{"
    &> opt (do_try (rep ident <& kwd "->"))
    <&> do_lazy simple_term
    <& kwd "}"
    <$> function
    | Some l, e -> List.fold_right (fun e a -> Abstraction (e, a)) l e
    | None, e -> e

  and simple_term () =
    do_lazy let_term <|> native_term <|> ident_term <|> do_lazy function_term

  let main = simple_term ()
end
