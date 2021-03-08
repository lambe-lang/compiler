open Lambe_ast

(*
module Gamma = struct
  open Type

  let neutral = Gamma ([], [], [], [])

  let combine (Gamma (k, t, s, w)) (Gamma (k', t', s', w')) =
    Gamma (k @ k', t @ t', s @ s', w @ w')

  let k_get = function Gamma (k, _, _, _) -> k

  let k_set k = Gamma (k, [], [], [])

  let t_get = function Gamma (_, t, _, _) -> t

  let t_set t = Gamma ([], t, [], [])

  let s_get = function Gamma (_, _, s, _) -> s

  let w_get = function Gamma (_, _, _, w) -> w
end
*)

module Substitution = struct
  (* Substituste v by r in t *)
  let rec substitute v r t =
    let open Type in
    let subst_field (n, t1) = n, substitute v r t1 in
    let rec subst_gamma (Gamma (kd, ty, si, wi)) =
      Gamma
        ( kd
        , List.map subst_field ty
        , List.map subst_field si
        , List.map subst_gamma wi )
    in
    match t with
    | Variable (a, _) when v = a -> r
    | Variable _ -> t
    | Arrow (t1, t2, s) -> Arrow (substitute v r t1, substitute v r t2, s)
    | Invoke (t1, t2, s) -> Invoke (substitute v r t1, substitute v r t2, s)
    | Apply (t1, t2, s) -> Apply (substitute v r t1, substitute v r t2, s)
    | Sum (t1, t2, s) -> Sum (substitute v r t1, substitute v r t2, s)
    | Forall (a, _, _, _) when a = v -> t
    | Forall (a, k, t1, s) -> Forall (a, k, substitute v r t1, s)
    | Exists (a, _, _, _) when a = v -> t
    | Exists (a, k, t1, s) -> Exists (a, k, substitute v r t1, s)
    | Rec (a, _, _) when v = a -> t
    | Rec (a, t1, s) -> Rec (a, substitute v r t1, s)
    | Const (a, l1, s) -> Const (a, List.map subst_field l1, s)
    | Trait (gamma, s) -> Trait (subst_gamma gamma, s)
end

module Checker = struct
  let rec subsume g t1 t2 =
    let open Type in
    let open Substitution in
    match t1, t2 with
    | _ when t1 = t2 -> true
    | Arrow (t1, t2, _), Arrow (t3, t4, _) -> subsume g t3 t1 && subsume g t2 t4
    | Invoke (t1, t2, _), Invoke (t3, t4, _) ->
      subsume g t3 t1 && subsume g t2 t4
    | t1, Sum (t2, t3, _) -> subsume g t1 t2 || subsume g t1 t3
    | Rec(a,t1,_),Rec(b,t2,s) -> subsume g t1 (substitute b (Variable (a,s)) t2) (* Fresh variable required *)
    | _ -> false
end
