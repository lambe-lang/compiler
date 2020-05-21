module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t) =
struct
  type t = string

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Parser

  (* open Lambe_ast.Entities *)

  let keywords =
    [
      "-{"
    ; "--"
    ]

  let commentBlock =
    let open Lambe_lexer.Lexeme in
    let rec content () =
      kwd "}"
      <$> (fun _ -> "")
      <|> (any <&> do_lazy content <$> (function f, s -> to_string f ^ s))
    in
    kwd "-{" &> do_lazy content

  let commentLine =
    let open Lambe_lexer.Lexeme in
    let rec content () =
      spaces
      <?> (function s -> List.exists (( = ) '\n') s)
      <$> (fun _ -> "")
      <|> (any <&> do_lazy content <$> (function f, s -> to_string f ^ s))
    in
    kwd "--" &> do_lazy content

  let comment =
    commentLine
    <|> commentBlock

  let main = comment
end
