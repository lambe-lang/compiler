open Lambe_ast.Type

module Monoid = struct
  let neutral = Gamma ([], [], [], [])

  let combine (Gamma (k, t, s, w)) (Gamma (k', t', s', w')) =
    Gamma (k @ k', t @ t', s @ s', w @ w')
end

module Helpers = struct
  let k_get = function Gamma (k, _, _, _) -> k

  let k_set = function k -> Gamma (k, [], [], [])

  let t_get = function Gamma (_, t, _, _) -> t

  let t_set = function t -> Gamma ([], t, [], [])

  let s_get = function Gamma (_, _, s, _) -> s

  let w_get = function Gamma (_, _, _, w) -> w
end

let empty = Monoid.neutral

let merge = Monoid.combine

let ( + ) = merge

let ( <? ) l1 l2 f =
  List.for_all
    (fun (n, t2) ->
      Option.fold ~none:false
        ~some:(fun (_, t1) -> f t1 t2)
        (List.find_opt (fun (m, _) -> n = m) l1))
    l2
