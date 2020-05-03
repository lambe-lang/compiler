type t =
  | Variable of string
  | Ident of string
  | Apply of t * t
  | Arrow of t * t
  | Forall of string * Kind.t * t
