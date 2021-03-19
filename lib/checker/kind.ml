open Lambe_ast

module Checker = struct
  let rec subsume k1 k2 =
    let open Kind in
    match k1, k2 with
    | _ when k1 = k2 -> true
    | Type _, Type _ -> true
    | Trait _, Type _ -> true
    | Arrow (k1, k2, _), Arrow (k3, k4, _) -> subsume k3 k1 && subsume k2 k4
    | Trait (l1, _), Trait (l2, _) -> Gamma.(l1 <? l2) subsume
    | _ -> false

  module Operator = struct
    let ( <? ) = subsume
  end
end
