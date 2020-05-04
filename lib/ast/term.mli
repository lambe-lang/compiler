type native =
  | Integer of int
  | Float of float
  | String of string
  | Char of char

type t =
  (* Native expressions *)
  | Native of native
  (* Lambda expression *)
  | Variable of string
  | Abstraction of string * t
  | Apply of t * t
  (* Let constructions *)
  | Ident of string
  | Let of string * t * t
  | LetImpl of Type.t list * Type.t option * t list * t
  (* Smart cast *)
  | When of string option * t * (Type.t * t) list
