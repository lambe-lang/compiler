module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t) =
struct
  type t = Lambe_ast.Type.t

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Type

  let keywords = [ "forall"; "self"; "("; ")"; ":"; "." ]

  let operator = operator <|> kwd "->"

  let self_type = kwd "self" <$> (fun i -> Variable i)

  let ident_type =
    ident
    <&> optrep (kwd "." &> ident)
    <$> (function i, [] -> Variable i | i, l -> Path (i :: l))

  let type_param =
    kwd "("
    &> ident
    <& kwd ":"
    <&> Kind.main
    <& kwd ")"
    <|> (ident <$> (fun n -> n, Lambe_ast.Kind.Type))

  let rec block_type () =
    kwd "("
    &> (operator <$> (fun i -> Variable i) <|> do_lazy forall_type)
    <& kwd ")"

  and simple_type () = self_type <|> ident_type <|> do_lazy block_type

  and apply_type () =
    do_lazy simple_type
    <&> optrep (do_lazy simple_type)
    <$> (fun (t, l) -> List.fold_left (fun a t -> Apply (a, t)) t l)

  and complex_type () =
    do_lazy apply_type
    <&> opt (operator <&> do_lazy complex_type)
    <$> function
    | k1, None -> k1 | k1, Some (op, k2) -> Apply (Apply (Variable op, k1), k2)

  and forall_type () =
    opt (kwd "forall" &> rep type_param <& kwd ".")
    <$> (function None -> [] | Some l -> l)
    <&> do_lazy complex_type
    <$> function
    | l, t -> List.fold_right (fun (n, k) a -> Forall (n, k, a)) l t

  let main = forall_type ()
end
