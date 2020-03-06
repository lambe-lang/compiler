(*
type native =
  | Int of int
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
  | When of string * t * (Type.t * t) list
*)

open Lambe_ast.Term

let pp_native ppf = function
  | Int i -> Format.fprintf ppf "%i" i
  | String s -> Format.fprintf ppf "\"%s\"" s
  | Char c -> Format.fprintf ppf "'%c'" c

let rec pp ppf = function
  | Native n -> Format.fprintf ppf "%a" pp_native n
  | Variable s -> Format.fprintf ppf "\"%s\"" s
  | Abstraction (n, t) -> Format.fprintf ppf "{%s -> %a}" n pp t
  | Apply (t1, t2) -> Format.fprintf ppf "(%a) %a" pp t1 pp t2
  | Ident id -> Format.fprintf ppf "%s" id
  | Let (n, t1, t2) -> Format.fprintf ppf "let %s = %a in %a" n pp t1 pp t2
  | LetImpl _ -> Format.fprintf ppf "TBD"
  | When _ -> Format.fprintf ppf "TBD"
