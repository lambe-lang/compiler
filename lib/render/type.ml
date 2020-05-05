open Lambe_ast.Type

let rec pp ppf = function
  | Variable s -> Format.fprintf ppf "%s" s
  | Apply (l, r) -> Format.fprintf ppf "(%a %a)" pp l pp r
  | Forall (n, k, t) -> Format.fprintf ppf "(%s:%a) -> %a" n Kind.pp k pp t
