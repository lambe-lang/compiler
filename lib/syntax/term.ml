module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t)
    (Type : Entry.API with type t = Lambe_ast.Type.t and type 'a p = 'a Parser.t) =
struct
  type t = Lambe_ast.Term.t

  type 'a p = 'a Parser.t

  open Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Term

  let keywords =
    [
      "self"; "("; ")"; "{"; "}"; "let"; "="; "->"; "in"; "."; "when"; "is"; "_"
    ]

  let native_term =
    integer
    <$> (fun f -> Native (Integer f))
    <|> (float <$> (fun f -> Native (Float f)))
    <|> (string <$> (fun f -> Native (String f)))
    <|> (char <$> (fun f -> Native (Char f)))

  let ident_term =
    ident
    <|> kwd "_"
    <|> kwd "="
    <|> operator
    <$> (fun f -> Variable f)
    <|> (do_try (kwd "(" <&> kwd ")") <$> (fun _ -> Variable "()"))

  let name = ident <|> (kwd "(" &> operator <& kwd ")") <|> kwd "_"

  let name_term = name <$> (fun f -> Variable f)

  let name_type = name <$> (fun f -> Lambe_ast.Type.Variable f)

  let rec let_term () =
    kwd "let"
    &> ident
    <&> optrep ident
    <& kwd "="
    <&> do_lazy apply_term
    <& kwd "in"
    <&> do_lazy apply_term
    <$> function
    | ((i, l), a), b ->
      Let (i, List.fold_right (fun t a -> Abstraction (t, a)) l a, b)

  and function_term () =
    kwd "{"
    &> opt (do_try (rep ident <& kwd "->"))
    <&> do_lazy apply_term
    <& kwd "}"
    <$> function
    | Some l, e -> List.fold_right (fun e a -> Abstraction (e, a)) l e
    | None, e -> Abstraction ("_", e)

  and when_term () =
    kwd "when"
    &> opt (kwd "let" &> ident <& kwd "=")
    <&> do_lazy apply_term
    <& kwd "{"
    <&> rep (do_lazy case_term)
    <& kwd "}"
    <$> (function (n, t), c -> When (n, t, c))

  and case_term () = kwd "is" &> name_type <& kwd "->" <&> do_lazy apply_term

  and block_term () =
    kwd "("
    &> (operator <$> (fun o -> Variable o) <|> do_lazy apply_term)
    <& kwd ")"

  and simple_term () =
    do_lazy let_term
    <|> native_term
    <|> ident_term
    <|> do_lazy function_term
    <|> do_lazy block_term
    <|> do_lazy when_term

  and invoque_term () =
    do_lazy simple_term
    <&> opt (kwd "." &> name_term)
    <$> (function o, Some m -> Apply (o, m) | o, None -> o)

  and apply_term () =
    do_lazy invoque_term
    <&> optrep @@ do_lazy invoque_term
    <$> (fun (e, l) -> List.fold_left (fun a t -> Apply (a, t)) e l)

  let main = apply_term ()
end
