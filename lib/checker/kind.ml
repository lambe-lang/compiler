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
    | Trait (l1, _), Trait (l2, _) ->
      List.for_all
        (fun (n, k2) ->
          match List.find_opt (fun (m, _) -> n = m) l1 with
          | Some (_, k1) -> subsume k1 k2
          | None -> false)
        l2
    | _ -> false

  module Operator = struct
    let ( <? ) = subsume
  end
end
