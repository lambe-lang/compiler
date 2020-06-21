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
  | LetImpl of Type.t * t
  (* Smart cast *)
  | When of (string option * t) * (Type.t * t) list
  (* Evolution *)
  | With of t * (string * t) list
