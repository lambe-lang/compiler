module TypeContext = struct
  let get =
    let open Lambe_ast.Type in
    function
    | Variable (_, s) -> s
    | Arrow (_, _, s) -> s
    | Invoke (_, _, s) -> s
    | Apply (_, _, s) -> s
    | Access (_, _, s) -> s
    | Union (_, _, s) -> s
    | Lambda (_, _, _, s) -> s
    | Forall (_, _, _, s) -> s
    | Exists (_, _, _, s) -> s
    | Rec (_, _, s) -> s
    | Const (_, _, s) -> s
    | Trait (_, s) -> s
end

module Substitution = struct
  (* Substituste v by r in t *)
  let substitute v r t =
    let open Lambe_ast.Type in
    let open List in
    let rec subs t =
      let subst_field (n, t1) = n, subs t1 in
      let rec subst_gamma (Gamma (kd, ty, si, wi)) =
        Gamma (kd, map subst_field ty, map subst_field si, map subst_gamma wi)
      in
      match t with
      | Variable (a, _) when v = a -> r
      | Variable _ -> t
      | Arrow (t1, t2, s) -> Arrow (subs t1, subs t2, s)
      | Invoke (t1, t2, s) -> Invoke (subs t1, subs t2, s)
      | Apply (t1, t2, s) -> Apply (subs t1, subs t2, s)
      | Access (t1, n, s) -> Access (subs t1, n, s)
      | Union (t1, t2, s) -> Union (subs t1, subs t2, s)
      | Lambda (a, _, _, _) when a = v -> t
      | Lambda (a, k, t1, s) -> Lambda (a, k, subs t1, s)
      | Forall (a, _, _, _) when a = v -> t
      | Forall (a, k, t1, s) -> Forall (a, k, subs t1, s)
      | Exists (a, _, _, _) when a = v -> t
      | Exists (a, k, t1, s) -> Exists (a, k, subs t1, s)
      | Rec (a, _, _) when v = a -> t
      | Rec (a, t1, s) -> Rec (a, subs t1, s)
      | Const (a, l1, s) -> Const (a, map subst_field l1, s)
      | Trait (gamma, s) -> Trait (subst_gamma gamma, s)
    in
    subs t
end

module Checker = struct
  let ( <$> ) = Option.map

  let ( >>= ) = Option.bind

  let rec check g t k =
    let open Kind.Checker.Operator in
    Option.fold ~none:false ~some:(fun k' -> k' <? k) (synthetize g t)

  and synthetize g t =
    let module K = Lambe_ast.Kind in
    let open Lambe_ast.Type in
    let open Kind.Checker.Operator in
    let open List in
    match t with
    | Variable (n, _) ->
      snd <$> find_opt (fun (m, _) -> n = m) Gamma.Helpers.(k_get g)
    | Arrow (_, _, s) -> Some (K.Type s)
    | Invoke (_, _, s) -> Some (K.Type s)
    | Apply (t1, t2, _) -> (
      synthetize g t1
      >>= function
      | K.Arrow (k', k, _) when check g t2 k' -> Some k | _ -> None )
    | Access (t1, n, _) -> (
      synthetize g t1
      >>= function
      | Trait (l, _) -> snd <$> find_opt (fun (m, _) -> n = m) l | _ -> None )
    | Union (t1, t2, _) -> (
      let r1 = synthetize g t1 in
      let r2 = r1 >>= (fun _ -> synthetize g t2) in
      match r1, r2 with
      | Some k1, Some k2 when k1 <? k2 -> Some k2
      | Some k1, Some k2 when k2 <? k1 -> Some k1
      | _ -> None )
    | Lambda (n, k, t, s) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      (fun k' -> K.Arrow (k, k', s)) <$> synthetize g t
    | Forall (n, k, t, _) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      synthetize g t
    | Exists (n, k, t, _) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      synthetize g t
    | Rec (n, t, s) ->
      let g = Gamma.(Helpers.k_set [ n, K.Type s ] + g) in
      synthetize g t
    | Const (_, _, s) -> if check g t (K.Type s) then Some (K.Type s) else None
    | Trait ((Gamma (k, t, s, w) as g'), l) ->
      if for_all (fun (_, t) -> check Gamma.(g + g') t (K.Type l)) t
         && for_all (fun (_, t) -> check Gamma.(g + g') t (K.Type l)) s
         && for_all (fun g -> check Gamma.empty (Trait (g, l)) (K.Type l)) w
      then
        Some (K.Trait (fold_left (fun k (Gamma (k', _, _, _)) -> k @ k') k w, l))
      else None

  let rec reduce g t =
    let open Lambe_ast.Type in
    let open Gamma in
    let open Substitution in
    let open List in
    let rec find_type n = function
      | [] -> None
      | Gamma (_, t, _, _) :: l -> (
        match find_opt (fun (m, _) -> n = m) t with
        | Some t -> Some t
        | None -> find_type n l )
    in
    match t with
    | Variable (n, _) ->
      snd <$> find_opt (fun (m, _) -> n = m) (Helpers.t_get g)
    | Apply (t1, t2, _) -> (
      reduce g t1
      >>= function
      | Lambda (a1, k1, t1, _) ->
        if check g t2 k1 then Some (substitute a1 t2 t1) else None
      | _ -> None )
    | Access (t, n, _) -> (
      reduce g t
      >>= function
      | Trait (g, _) -> (
        match find_opt (fun (m, _) -> n = m) (Helpers.t_get g) with
        | Some (_, t) -> Some t
        | None -> snd <$> find_type n (Helpers.w_get g) )
      | _ -> None )
    | _ -> Some t

  (* Should return a State *)
  let rec subsume g t1 t2 v =
    let module K = Lambe_ast.Kind in
    let open Lambe_ast.Type in
    let open Gamma in
    let open Context in
    let open Substitution in
    let open Kind.Checker.Operator in
    let open List in
    let print_subtype = Lambe_render.Type.Render.subtype Format.err_formatter in
    let _ = print_subtype t1 t2
    and _ = print_string "\n" in
    match t1, t2 with
    | _ when t1 = t2 -> check g t1 (K.Type (TypeContext.get t1)), v
    (* Apply section *)
    | t, Apply (Forall (a1, _, t1, _), t2, _) ->
      subsume g t (substitute a1 t2 t1) v
    | Apply (Forall (a1, _, t1, _), t2, _), t ->
      subsume g (substitute a1 t2 t1) t v
    (* Arrow *)
    | Arrow (t1, t2, _), Arrow (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    (* Invoke *)
    | Invoke (t1, t2, _), Invoke (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    (* Union *)
    | Union (t1, t2, _), t3 ->
      let b1, v1 = subsume g t1 t3 v in
      if b1
      then
        let b2, v2 = subsume g t2 t3 v1 in
        b1 && b2, v2
      else false, v
    | t1, Union (t2, t3, _) ->
      let b1, v1 = subsume g t1 t2 v in
      if b1
      then b1, v1
      else
        let b2, v2 = subsume g t1 t3 v1 in
        b2, v2
    (* Rec *)
    | Rec (a1, t1, s1), Rec (a2, t2, s2) ->
      let n, v = Variables.fresh v in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    | Rec (a1, t1', _), t2 -> subsume g (substitute a1 t1 t1') t2 v
    | t1, Rec (a2, t2', _) -> subsume g t1 (substitute a2 t2 t2') v
    (* Forall *)
    | Forall (a1, k1, t1, s1), Forall (a2, k2, t2, s2) ->
      if k2 <? k1
      then
        let n, v = Variables.fresh v in
        let g = Helpers.k_set [ n, k2 ] + g in
        let t1 = substitute a1 (Variable (n, s1)) t1 in
        let t2 = substitute a2 (Variable (n, s2)) t2 in
        subsume g t1 t2 v
      else false, v
    (* Exists *)
    | Exists (a1, k1, t1, s1), Exists (a2, k2, t2, s2) ->
      let n, v = Variables.fresh v in
      let g = Helpers.k_set [ n, k1 ] + g in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      if k1 <? k2 then subsume g t1 t2 v else false, v
    (* Constructor *)
    | Const (n1, l1, _), Const (n2, l2, _) when n1 = n2 ->
      Gamma.(l1 <? l2) (fun t1 t2 -> fst (subsume g t1 t2 v)), v
    (* Trait *)
    | Trait (Gamma (k, t, s, _), l), Trait (Gamma (k', t', s', _), l') ->
      ( Trait (k, l) <? Trait (k', l')
        && Gamma.(t <? t') (fun t1 t2 -> fst (subsume g t1 t2 v))
        && Gamma.(s <? s') (fun t1 t2 -> fst (subsume g t1 t2 v))
      , v )
    | t1, t2 -> (
      match reduce g t1 with
      | Some t1' when t1' != t1 -> subsume g t1' t2 v
      | _ -> (
        match reduce g t2 with
        | Some t2' when t2' != t2 -> subsume g t1 t2' v
        | _ -> false, v ) )

  module Operator = struct
    let ( <:?> ) t1 t2 g = check g t1 t2

    let ( <? ) t1 t2 c g = subsume g t1 t2 c

    let ( |- ) g f = f g
  end
end
