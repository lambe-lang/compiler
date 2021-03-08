(*

  ------
  k ⊆κ ⋆

  k1 ⊆κ k3    k2 ⊆κ k4
  --------------------
  k1 → k2 ⊆κ k3 → k4

*)

open Lambe_ast

module Checker = struct
  let rec subsume k1 k2 =
    let open Kind in
    match k1, k2 with
    | _, Type _ -> true
    | Arrow (k1, k2, _), Arrow (k3, k4, _) -> subsume k1 k3 && subsume k2 k4
    | _ -> false
end
