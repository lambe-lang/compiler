module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t) =
struct
  open Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Type

  let keywords = [ "self"; "("; ")" ]

  (*
     type ::=
            simple_type+

     simple_type ::=
            "self"
            IDENT
            "(" paren_type

     paren_type ::=
            OPERATOR ")"
            complex_type ")"

     complex_type ::=
            simple_type+ (OPERATOR complex_kind)?
  *)

  let operator = operator <|> kwd "->"

  let rec simple_type () =
    kwd "self"
    <|> ident
    <$> (fun i -> Variable i)
    <|> (kwd "(" &> do_lazy paren_type)

  and paren_type () =
    operator
    <& kwd ")"
    <$> (fun i -> Ident i)
    <|> (do_lazy complex_type <& kwd ")")

  and complex_type () =
    do_lazy simple_type
    <&> optrep (do_lazy simple_type)
    <$> (fun (t, l) -> List.fold_left (fun a t -> Apply (a, t)) t l)
    <&> opt (operator <&> do_lazy complex_type)
    <$> function
    | k1, None -> k1 | k1, Some (op, k2) -> Apply (Apply (Ident op, k1), k2)

  let main = complex_type ()
end
