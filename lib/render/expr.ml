open Lambe_ast

module Render = struct
  let rec pp ppf =
    let open Expr in
    (function _ -> Format.fprintf ppf "?")
end
