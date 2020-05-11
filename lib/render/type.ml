open Lambe_ast.Type

let rec pp_ident ppf = function
  | [] -> ()
  | [ s ] -> Format.fprintf ppf "%s" s
  | s :: l -> Format.fprintf ppf "%s.%a" s pp_ident l

let rec pp ppf = function
  | Path l -> pp_ident ppf l
  | Variable s -> Format.fprintf ppf "%s" s
  | Apply (l, r) -> Format.fprintf ppf "(%a %a)" pp l pp r
  | Forall (n, k, t) -> Format.fprintf ppf "forall (%s:%a).%a" n Kind.pp k pp t
