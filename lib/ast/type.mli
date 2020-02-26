type native =
  | Int
  | String
  | Char

type t =
  | Native of native
  | Variable of string
  | Ident of string
  | Apply of t * t
  | Arrow of t * t
  | Forall of string * Kind.t * t
