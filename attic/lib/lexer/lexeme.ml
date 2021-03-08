type t =
  | Spaces of char list
  | Integer of int
  | Float of float
  | String of string
  | Char of char
  | Ident of string
  | Operator of string
  | Keyword of string
  | Unknown of char

let to_string = function
  | Spaces s -> Transept.Utils.string_of_chars s
  | Integer s -> string_of_int s
  | Float s -> string_of_float s
  | String s | Ident s | Operator s | Keyword s -> s
  | Char c | Unknown c -> Transept.Utils.string_of_chars [ c ]
