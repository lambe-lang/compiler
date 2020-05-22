module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t)
    (Type : Entry.API with type t = Lambe_ast.Type.t and type 'a p = 'a Parser.t) =
struct
  type t = Lambe_ast.Term.t

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Term

  let keywords =
    [
      "is"
    ; "let"
    ; "self"
    ; "when"
    ; "where"
    ; "with"
    ; "("
    ; ")"
    ; "{"
    ; "}"
    ; "="
    ; "->"
    ; "in"
    ; "."
    ; "_"
    ]

  let native_term =
    integer
    <$> (fun f -> Literal (Integer f))
    <|> (float <$> (fun f -> Literal (Float f)))
    <|> (string <$> (fun f -> Literal (String f)))
    <|> (char <$> (fun f -> Literal (Char f)))

  let ident_term =
    ident
    <|> kwd "_"
    <|> kwd "="
    <|> kwd "self"
    <|> kwd "--"
    <|> operator
    <$> (fun f -> Variable f)
    <|> (do_try (kwd "(" <&> kwd ")") <$> (fun _ -> Variable "()"))

  let name = ident <|> (kwd "(" &> operator <& kwd ")") <|> kwd "_"

  let name_term = name <$> (fun f -> Variable f)

  let name_type = name <$> (fun f -> Lambe_ast.Type.Variable f)

  let rec let_term () =
    kwd "let"
    &> ident
    <&> optrep (ident <|> kwd "_")
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
    rep
      (kwd "when" &> opt (kwd "let" &> ident <& kwd "=") <&> do_lazy simple_term)
    <&> rep (do_lazy case_term)
    <$> (function n, l -> When (n, l))

  and case_term () =
    rep (kwd "is" &> name_type) <& kwd "->" <&> do_lazy apply_term

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
    <&> optrep (kwd "with" &> ident <& kwd "=" <&> do_lazy apply_term)
    <$> (function t, [] -> t | t, l -> With (t, l))

  and invoque_term () =
    do_lazy simple_term
    <&> opt (kwd "." &> name_term)
    <$> (function o, Some m -> Apply (o, m) | o, None -> o)

  and apply_term () =
    do_lazy invoque_term
    <&> optrep @@ do_lazy invoque_term
    <$> (fun (e, l) -> List.fold_left (fun a t -> Apply (a, t)) e l)

  let where_term =
    kwd "where"
    &> ident
    <&> optrep (ident <|> kwd "_")
    <& kwd "="
    <&> do_lazy apply_term
    <$> function
    | (i, l), a ->
      (fun b -> Let (i, List.fold_right (fun t a -> Abstraction (t, a)) l a, b))

  let term () =
    do_lazy apply_term
    <&> optrep where_term
    <$> (function t, w -> List.fold_right (fun l a -> l a) w t)

  let main = term ()
end
