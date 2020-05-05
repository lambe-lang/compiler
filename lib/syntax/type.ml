module Make_via_parser (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t) = struct
  open Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Type

  let keywords = [ "forall"; "self"; "("; ")"; ":"; "." ]

  let operator = operator <|> kwd "->"

  let self_type = kwd "self" <$> (fun i -> Variable i)

  let ident_type = ident <$> (fun i -> Variable i)

  let rec block_type () =
    kwd "(" &> (operator <$> (fun i -> Ident i) <|> do_lazy forall_type) <& kwd ")"

  and simple_type () = self_type <|> ident_type <|> do_lazy block_type

  and apply_type () =
    do_lazy simple_type
    <&> optrep (do_lazy simple_type)
    <$> (fun (t, l) -> List.fold_left (fun a t -> Apply (a, t)) t l)

  and complex_type () =
    do_lazy apply_type
    <&> opt (operator <&> do_lazy complex_type)
    <$> (function k1, None -> k1 | k1, Some (op, k2) -> Apply (Apply (Ident op, k1), k2))

  and forall_type () =
    opt (kwd "forall" &> rep ident <& kwd ".")
    <$> (function None -> [] | Some l -> l)
    <&> do_lazy complex_type
    <$> (function l, t -> List.fold_right (fun n a -> Forall (n, Lambe_ast.Kind.Type, a)) l t)

  let main = forall_type ()
end
