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

val to_string : t -> string
