module Lexeme = struct
  type t =
    | Float of float
    | String of string
    | Char of char
    | Ident of string
    | Operator of string
    | Keyword of string

  let pp ppf = function
    | Float value -> Format.fprintf ppf "%f" value
    | String value -> Format.fprintf ppf "\"%s\"" value
    | Char value -> Format.fprintf ppf "'%c'" value
    | Ident value -> Format.fprintf ppf "%s" value
    | Operator value -> Format.fprintf ppf "%s" value
    | Keyword value -> Format.fprintf ppf "%s" value

  let to_string = Format.asprintf "%a" pp
end

module Make (Parser : Transept_specs.PARSER with type e = char) = struct
  open Transept_utils.Utils
  open Lexeme

  open Transept_extension.Literals.Make (Parser)

  let operator =
    let open Parser in
    rep
      (in_list
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
         ])
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
    <|> (keywords <$> (fun e -> Keyword e))
    <|> (float <$> (fun e -> Float e))
    <|> (string <$> (fun e -> String e))
    <|> (char <$> (fun e -> Char e))
    <|> (ident <$> (fun e -> Ident e))
    <& skipped

  let tokenizer_with_spaces l = tokenizer spaces l
end

module Token (Parser : Transept_specs.PARSER with type e = Lexeme.t) = struct
  open Parser
  open Lexeme

  let float = any >>= (function Float f -> return f | _ -> fail)

  let string = any >>= (function String s -> return s | _ -> fail)

  let char = any >>= (function Char c -> return c | _ -> fail)

  let ident = any >>= (function Ident i -> return i | _ -> fail)

  let operator = any >>= (function Operator i -> return i | _ -> fail)

  let kwd s = any >>= (fun a -> if a = Keyword s then return s else fail)
end
