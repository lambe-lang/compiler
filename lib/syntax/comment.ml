module Make_via_parser
    (Parser : Transept.Specs.PARSER with type e = Lambe_lexer.Lexeme.t) =
struct
  type t = string

  type 'a p = 'a Parser.t

  open Lambe_lexer.Lexer.Token (Parser)

  open Transept_utils.Utils
  open Parser

  let keywords = [ "-{"; "--" ]

  let any = Lambe_lexer.Lexeme.(any <$> to_string)

  let exit_block d () = kwd "}" <?> (fun _ -> d = 0) <$> constant ""

  let escape_block c d () =
    any
    <?> (fun s -> s = "\\{" || s = "\\}")
    <&> do_lazy (c d)
    <$> uncurry ( ^ )

  let open_block c d () =
    any <?> ( = ) "{" <&> do_lazy (c (d + 1)) <$> uncurry ( ^ )

  let close_block c d () =
    any <?> ( = ) "}" <&> do_lazy (c (d - 1)) <$> uncurry ( ^ )

  let any_block c d () = any <&> do_lazy (c d) <$> uncurry ( ^ )

  let commentBlock =
    let rec content d () =
      do_lazy (exit_block d)
      <|> do_lazy (escape_block content d)
      <|> do_lazy (close_block content d)
      <|> do_lazy (open_block content d)
      <|> do_lazy (any_block content d)
    in
    kwd "-{" &> do_lazy (content 0)

  let commentLine =
    let rec content () =
      spaces
      <?> (function s -> List.exists (( = ) '\n') s)
      <$> (fun _ -> "")
      <|> (any <&> do_lazy content <$> uncurry ( ^ ))
    in
    kwd "--" &> do_lazy content

  let comment = commentLine <|> commentBlock

  let main = comment
end
