open Lambe_ast.Type

let pp_native ppf = function
  | Int -> Format.fprintf ppf "int"
  | String -> Format.fprintf ppf "string"
  | Char -> Format.fprintf ppf "char"

let rec pp ppf = function
  | Native n -> Format.fprintf ppf "%a" pp_native n
  | Variable s -> Format.fprintf ppf "%s" s
  | Ident id -> Format.fprintf ppf "%s" id
  | Apply (l, r) -> Format.fprintf ppf "(%a) %a" pp l pp r
  | Arrow (l, r) -> Format.fprintf ppf "(%a) -> %a" pp l pp r
  | Forall (n, k, t) -> Format.fprintf ppf "(%s:%a) -> %a" n Kind.pp k pp t
