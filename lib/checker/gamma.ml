open Lambe_ast.Type

module Helpers = struct
  let k_get = function Gamma (k, _, _, _) -> k

  let k_set = function k -> Gamma (k, [], [], [])

  let t_get = function Gamma (_, t, _, _) -> t

  let t_set = function t -> Gamma ([], t, [], [])

  let s_get = function Gamma (_, _, s, _) -> s

  let w_get = function Gamma (_, _, _, w) -> w
end

module Monoid = struct
  let neutral = Gamma ([], [], [], [])

  let combine (Gamma (k, t, s, w)) (Gamma (k', t', s', w')) =
    Gamma (k @ k', t @ t', s @ s', w @ w')
end
