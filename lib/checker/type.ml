open Lambe_ast

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
  type 'a state = Variable.t -> bool * Variable.t

  (* Should return a State *)
  let rec subsume g t1 t2 v =
    let open Type in
    let open Gamma in
    let open Substitution in
    match t1, t2 with
    | _ when t1 = t2 -> true, v
    | Arrow (t1, t2, _), Arrow (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    | Invoke (t1, t2, _), Invoke (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    | t1, Sum (t2, t3, _) ->
      let b1, v1 = subsume g t1 t2 v in
      if b1
      then b1, v1
      else
        let b2, v2 = subsume g t1 t3 v1 in
        b2, v2
    | Rec (a1, t1, s1), Rec (a2, t2, s2) ->
      let n, v = Variable.fresh v in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    | Rec (a1, t1', _), t2 -> subsume g (substitute a1 t1 t1') t2 v
    | t1, Rec (a2, t2', _) -> subsume g t1 (substitute a2 t2 t2') v
    | Const (n1, _, _), Const (n2, _, _) when n1 = n2 -> false, v
    | Forall (a1, k1, t1, s1), Forall (a2, _ (*k2*), t2, s2) ->
      (* Relation between k1 et k2 *)
      let n, v = Variable.fresh v in
      let g = Monoid.combine g (Helpers.k_set [ n, k1 ]) in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    | Exists (a1, k1, t1, s1), Exists (a2, _ (*k2*), t2, s2) ->
      (* Relation between k1 et k2 *)
      let n, v = Variable.fresh v in
      let g = Monoid.combine g (Helpers.k_set [ n, k1 ]) in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    | Trait (_, _), Trait (_, _) -> false, v
    | _ -> false, v
end
