open Lambe_ast

module Render = struct
  let rec pp ppf =
    let open Kind in
    function
    | Arrow (l, r, _) -> Format.fprintf ppf "(%a) -> %a" pp l pp r
    | Type _ -> Format.fprintf ppf "type"
    | Trait _ -> Format.fprintf ppf "trait"
end
