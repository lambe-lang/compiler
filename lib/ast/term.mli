type literal =
  | Integer of int
  | Float of float
  | String of string
  | Char of char

type t =
  (* Literal expressions *)
  | Literal of literal
  (* Lambda expression *)
  | Variable of string
  | Abstraction of string * t
  | Apply of t * t
  (* Let constructions *)
  | Let of string * t * t
  (* Smart cast *)
  | When of (string option * t) list * (Type.t list * t) list
  | With of t * (string * t) list
