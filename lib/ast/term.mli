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
  | Let of string * t * t
  (* Smart cast *)
  | When of string option * t * (Type.t * t) list
