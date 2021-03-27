open Lambe_ast

module Render = struct
  let rec pp ppf =
    let open Kind in
    function
    | Arrow (l, r, _) -> Format.fprintf ppf "(%a) -> %a" pp l pp r
    | Type _ -> Format.fprintf ppf "*"
    | Trait (l, _) -> Format.fprintf ppf "trait { %a }" pp_list l

  and pp_list ppf = function
    | [] -> ()
    | (n, k) :: l -> Format.fprintf ppf "kind %s = %a %a" n pp k pp_list l
end
