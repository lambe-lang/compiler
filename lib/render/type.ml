open Lambe_ast.Type

let is_alpha s =
  let c = s.[0] in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'

let rec pp_ident ppf = function
  | [] -> ()
  | [ s ] -> Format.fprintf ppf "%s" s
  | s :: l -> Format.fprintf ppf "%s.%a" s pp_ident l

let rec pp ppf = function
  | Path l -> pp_ident ppf l
  | Variable s when is_alpha s -> Format.fprintf ppf "%s" s
  | Variable s -> Format.fprintf ppf "(%s)" s
  | Apply (Apply (Variable s, (Apply (Apply (Variable _, _), _) as l)), r)
    when is_alpha s == false ->
    Format.fprintf ppf "(%a) %s %a" pp l s pp r
  | Apply (Apply (Variable s, l), r) when is_alpha s == false ->
    Format.fprintf ppf "%a %s %a" pp l s pp r
  | Apply (l, (Apply (_, _) as r)) -> Format.fprintf ppf "%a (%a)" pp l pp r
  | Apply (l, r) -> Format.fprintf ppf "%a %a" pp l pp r
  | Forall (n, k, t) -> Format.fprintf ppf "forall (%s:%a).%a" n Kind.pp k pp t
