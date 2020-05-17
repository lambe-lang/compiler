module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t)
    (Kind : Entry.API with type t = Lambe_ast.Kind.t and type 'a p = 'a Parser.t)
    (Type : Entry.API with type t = Lambe_ast.Type.t and type 'a p = 'a Parser.t)
    (Term : Entry.API with type t = Lambe_ast.Term.t and type 'a p = 'a Parser.t) =
struct
  type t = Lambe_ast.Entity.t

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Parser
  open Lambe_ast.Entity

  (* open Lambe_ast.Entities *)

  let keywords =
    [
      "comment"
    ; "kind"
    ; "type"
    ; "data"
    ; "sig"
    ; "def"
    ; "trait"
    ; "impl"
    ; "("
    ; ")"
    ; "{"
    ; "}"
    ; "="
    ; ":"
    ; ";"
    ; "|"
    ; "for"
    ; "with"
    ; "forall"
    ; "."
    ]

  let kind_name = ident <|> (kwd "(" &> (operator <|> kwd "->") <& kwd ")")

  let operator = operator <|> (do_try (kwd "(" &> kwd ")") <$> (fun _ -> "()"))

  let sig_name = ident <|> (kwd "(" &> (operator <|> kwd "=") <& kwd ")")

  let type_param =
    kwd "("
    &> (ident <|> kwd "_")
    <& kwd ":"
    <&> Kind.main
    <& kwd ")"
    <|> (ident <|> kwd "_" <$> (fun n -> n, Lambe_ast.Kind.Type))

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

  let for_directive = opt (kwd "for" &> Type.main)

  let with_directive = optrep (kwd "with" &> Type.main)

  let sig_entity =
    kwd "sig"
    &> sig_name
    <& kwd ":"
    <&> Type.main
    <&> for_directive
    <&> with_directive
    <$> (function ((n, t), f), w -> Sig (n, t, f, w))

  let def_entity =
    kwd "def"
    &> sig_name
    <&> optrep (ident <|> kwd "_")
    <& kwd "="
    <&> Term.main
    <$> function
    | (n, l), t ->
      Def (n, List.fold_right (fun e a -> Lambe_ast.Term.Abstraction (e, a)) l t)

  (** TODO *)
  let comment =
    let rec content () = is_kwd "}" <|> (any &> do_lazy content) in
    kwd "comment"
    &> kwd "{"
    &> do_lazy content
    <$> (fun _ -> Comment [ Lambe_ast.Comment.Block "TODO" ])

  let rec trait_entity () =
    kwd "trait"
    &> sig_name
    <&> optrep type_param
    <&> for_directive
    <&> with_directive
    <&> ( opt (kwd "{" &> optrep (do_lazy entity) <& kwd "}")
        <$> (function None -> [] | Some l -> l) )
    <$> (function (((n, p), f), w), e -> Trait (n, p, f, w, e))

  and impl_entity () =
    kwd "impl"
    &> ( opt (kwd "forall" &> optrep type_param <& kwd ".")
       <$> (function None -> [] | Some l -> l) )
    <&> Type.main
    <&> for_directive
    <&> with_directive
    <&> ( opt (kwd "{" &> optrep (do_lazy entity) <& kwd "}")
        <$> (function None -> [] | Some l -> l) )
    <$> (function (((p, t), f), w), e -> Impl (p, t, f, w, e))

  and entity () =
    comment
    <|> data_entity
    <|> type_entity
    <|> kind_entity
    <|> sig_entity
    <|> def_entity
    <|> do_lazy trait_entity
    <|> do_lazy impl_entity

  let main = entity ()
end
