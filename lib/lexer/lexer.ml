module Make (Parser : Transept_specs.PARSER with type e = char) = struct
  open Transept_utils.Utils
  open Lexeme

  open Transept_extension.Literals.Make (Parser)

  let first =
    [
      '<'
    ; '>'
    ; '+'
    ; '-'
    ; '*'
    ; '/'
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
    ; '.'
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

  let tokenizer s l =
    let open Parser in
    let keywords =
      List.fold_left (fun p e -> p <|> atoms e) fail
      @@ List.map chars_of_string l
      <$> string_of_chars
    and skipped = optrep s <$> constant () in
    skipped
    &> ( operator
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
    <& skipped

  let tokenizer_with_spaces l = tokenizer spaces l
end

module Token (Parser : Transept_specs.PARSER with type e = Lexeme.t) = struct
  open Parser
  open Lexeme

  let integer = any >>= (function Integer f -> return f | _ -> fail)

  let float = any >>= (function Float f -> return f | _ -> fail)

  let string = any >>= (function String s -> return s | _ -> fail)

  let char = any >>= (function Char c -> return c | _ -> fail)

  let ident = any >>= (function Ident i -> return i | _ -> fail)

  let operator = any >>= (function Operator i -> return i | _ -> fail)

  let kwd s = any >>= (fun a -> if a = Keyword s then return s else fail)
end
