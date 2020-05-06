module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t)
    (Type : Entry.API with type t = Lambe_ast.Type.t and type 'a p = 'a Parser.t)
    (Term : Entry.API with type t = Lambe_ast.Term.t and type 'a p = 'a Parser.t) =
struct
  type t = Lambe_ast.Entity.t

  type 'a p = 'a Parser.t

  open Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Entity

  (* open Lambe_ast.Entities *)

  let keywords =
    [
      "kind"
    ; "type"
    ; "data"
    ; "sig"
    ; "def"
    ; "trait"
    ; "("
    ; ")"
    ; "="
    ; ":"
    ; ";"
    ; "|"
    ; "for"
    ; "with"
    ]

  let kind_name = ident <|> (kwd "(" &> (operator <|> kwd "->") <& kwd ")")

  let sig_name = ident <|> (kwd "(" &> (operator <|> kwd "=") <& kwd ")")

  let type_param =
    kwd "("
    &> ident
    <& kwd ":"
    <&> Kind.main
    <& kwd ")"
    <|> (ident <$> (fun n -> n, Lambe_ast.Kind.Type))

  let data_attributes =
    let data = ident <& kwd ":" <&> Type.main in
    to_list (data <&> optrep (kwd ";" &> data)) <|> return []

  let data_entity =
    kwd "data"
    &> sig_name
    <&> optrep type_param
    <&> ( opt (kwd "{" &> data_attributes <& kwd "}")
        <$> (function None -> [] | Some l -> l) )
    <$> (function (n, p), t -> Data (n, p, t))

  let type_entity =
    kwd "type"
    &> sig_name
    <&> optrep type_param
    <& kwd "="
    <&> to_list (Type.main <&> optrep (kwd "|" &> Type.main))
    <$> function
    | (n, p), [ t ] -> Type (n, p, t) | (n, p), l -> Enum (n, p, l)

  let kind_entity =
    kwd "kind"
    &> kind_name
    <& kwd "="
    <&> Kind.main
    <$> (function n, t -> Kind (n, t))

  let sig_entity =
    kwd "sig"
    &> sig_name
    <& kwd ":"
    <&> Type.main
    <&> opt (kwd "for" &> Type.main)
    <&> optrep (kwd "with" &> Type.main)
    <$> (function ((n, t), f), w -> Sig (n, t, f, w))

  let def_entity =
    kwd "def"
    &> sig_name
    <&> optrep ident
    <& kwd "="
    <&> Term.main
    <$> function
    | (n, l), t ->
      Def (n, List.fold_right (fun e a -> Lambe_ast.Term.Abstraction (e, a)) l t)

  let entity () =
    data_entity <|> type_entity <|> kind_entity <|> sig_entity <|> def_entity

  let main = entity ()
end
