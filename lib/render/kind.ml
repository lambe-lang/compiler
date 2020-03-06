open Lambe_ast.Kind

let rec pp ppf = function
  | Arrow (l, r) -> Format.fprintf ppf "(%a) -> %a" pp l pp r
  | Type -> Format.fprintf ppf "type"
