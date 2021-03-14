open Lambe_ast

module Render = struct
  let rec pp ppf =
    let open Type in
    function
    | Variable (n, _) -> Format.fprintf ppf "%s" n
    | Arrow (l, r, _) -> Format.fprintf ppf "(%a) -> (%a)" pp l pp r
    | Invoke (l, r, _) -> Format.fprintf ppf "(%a) @> (%a)" pp l pp r
    | Union (l, r, _) -> Format.fprintf ppf "(%a) | (%a)" pp l pp r
    | _ -> Format.fprintf ppf "%s" "?"

  let subtype ppf t1 t2 = Format.fprintf ppf "(%a) <? (%a) @." pp t1 pp t2
end
