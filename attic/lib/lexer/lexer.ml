module Make (Parser : Transept_specs.PARSER with type e = char) = struct
  open Transept.Utils
  open Lexeme

  open Transept.Extension.Literals.Make (Parser)

  let first =
    [
      '<'
    ; '>'
    ; '+'
    ; '-'
    ; '*'
    ; '/'
    ; '\\'
    ; ','
    ; '~'
    ; '='
    ; '|'
    ; '&'
    ; '_'
    ; '['
    ; ']'
    ; ':'
    ; '$'
    ; '@'
    ; '!'
    ; '.'
    ; '{'
    ; '}'
    ]

  let next = first

  let operator =
    let open Parser in
    to_list (in_list first <&> optrep (in_list next)) <$> string_of_chars

  let ident =
    let open Parser in
    to_list
      ( alpha
      <|> atom '_'
      <&> optrep (alpha <|> digit <|> in_list [ '_'; '?'; '$' ]) )
    <$> string_of_chars

  let space = Parser.in_list [ ' '; '\t'; '\r'; '\n' ]

  let tokenizer l =
    let open Parser in
    let keywords =
      List.fold_left (fun p e -> p <|> atoms e) fail
      @@ List.map chars_of_string l
      <$> string_of_chars
    in
    rep space
    <$> (function s -> Spaces s)
    <|> ( operator
        <$> fun e ->
        if List.exists (fun k -> e = k) l then Keyword e else Operator e )
    <|> ( ident
        <$> fun e ->
        if List.exists (fun k -> e = k) l then Keyword e else Ident e )
    <|> (keywords <$> (fun e -> Keyword e))
    <|> (do_try float <$> (fun e -> Float e))
    <|> (integer <$> (fun e -> Integer e))
    <|> (string <$> (fun e -> String e))
    <|> (char <$> (fun e -> Char e))
    <|> (any <$> (fun e -> Unknown e))
end

module Token (Parser : Transept_specs.PARSER with type e = Lexeme.t) = struct
  open Parser
  open Lexeme

  let spaces = any >>= (function Spaces f -> return f | _ -> fail)

  let lexeme = opt spaces &> any <& opt spaces

  let integer = lexeme >>= (function Integer f -> return f | _ -> fail)

  let float = lexeme >>= (function Float f -> return f | _ -> fail)

  let string = lexeme >>= (function String s -> return s | _ -> fail)

  let char = lexeme >>= (function Char c -> return c | _ -> fail)

  let ident = lexeme >>= (function Ident i -> return i | _ -> fail)

  let operator = lexeme >>= (function Operator i -> return i | _ -> fail)

  let kwd s = lexeme >>= (fun a -> if a = Keyword s then return s else fail)

  let is_kwd s = lexeme >>= (fun a -> if a = Keyword s then return a else fail)
end
