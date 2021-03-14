module Substitution = struct
  (* Substituste v by r in t *)
  let rec substitute v r t =
    let open Lambe_ast.Type in
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
    | Access (t1, n, s) -> Access (substitute v r t1, n, s)
    | Union (t1, t2, s) -> Union (substitute v r t1, substitute v r t2, s)
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
  type 'a state = Context.Variables.t -> bool * Context.Variables.t

  let rec check g t k =
    let open Kind.Checker.Operator in
    Option.fold ~none:false ~some:(fun k' -> k' <? k) (synthetize g t)

  and synthetize g t =
    let open Gamma in
    let open Lambe_ast.Type in
    let module K = Lambe_ast.Kind in
    match t with
    | Variable (n, _) -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.k_get g) with
      | Some (_, k') -> Some k'
      | _ -> None )
    | Arrow (_, _, s) -> Some (K.Type s)
    | Invoke (_, _, s) -> Some (K.Type s)
    | Apply (t1, t2, _) -> (
      match synthetize g t1 with
      | Some (K.Arrow (k', k, _)) -> if check g t2 k' then Some k else None
      | _ -> None )
    | Access (t1, n, _) -> (
      match synthetize g t1 with
      | Some (Trait (l, _)) -> (
        match List.find_opt (fun (m, _) -> n = m) l with
        | Some (_, k) -> Some k
        | None -> None )
      | _ -> None )
    | Union (t1, t2, _) -> (
      match synthetize g t1 with
      | Some k -> if check g t2 k then Some k else None
      | None -> None )
    | Forall (n, k, t, s) -> (
      let g = Helpers.k_set [ n, k ] + g in
      match synthetize g t with
      | Some k' -> Some (K.Arrow (k, k', s))
      | None -> None )
    | Exists (n, k, t, _) ->
      let g = Helpers.k_set [ n, k ] + g in
      synthetize g t
    | Rec (n, t, s) ->
      let g = Helpers.k_set [ n, K.Type s ] + g in
      synthetize g t
    | Const (_, _, s) -> if check g t (K.Type s) then Some (K.Type s) else None
    | Trait ((Gamma (k, t, s, w) as g), l) ->
      if List.for_all (fun (_, t) -> check g t (K.Type l)) t
         && List.for_all (fun (_, t) -> check g t (K.Type l)) s
         && List.for_all (fun g -> check empty (Trait (g, l)) (K.Type l)) w
      then
        Some
          (K.Trait
             (List.fold_left (fun k (Gamma (k', _, _, _)) -> k @ k') k w, l) )
      else None

  (* Should return a State *)
  let rec subsume g t1 t2 v =
    let open Lambe_ast.Type in
    let open Gamma in
    let open Context in
    let open Substitution in
    let print_subtype = Lambe_render.Type.Render.subtype Format.err_formatter in
    let _ = print_subtype t1 t2
    and _ = print_string "\n" in
    match t1, t2 with
    | _ when t1 = t2 -> true, v
    (* Apply section *)
    | t, Apply (Forall (a1, _, t1, _), t2, _) ->
      subsume g t (substitute a1 t2 t1) v
    | Apply (Forall (a1, _, t1, _), t2, _), t ->
      subsume g (substitute a1 t2 t1) t v
    | t, Apply (Variable (n, _), t2, s) -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g) with
      | Some (_, t1) -> subsume g t (Apply (t1, t2, s)) v
      | _ -> false, v )
    | Apply (Variable (n, _), t2, s), t -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g) with
      | Some (_, t1) -> subsume g (Apply (t1, t2, s)) t v
      | _ -> false, v )
    (* Access Section *)
    | t, Access (Trait (g', _), n, _) -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g') with
      | Some (_, t2) -> subsume g t t2 v
      | _ -> false, v )
    | Access (Trait (g', _), n, _), t -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g') with
      | Some (_, t1) -> subsume g t1 t v
      | _ -> false, v )
    | t, Access (Variable (n, _), m, s) -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g) with
      | Some (_, t1) -> subsume g t (Access (t1, m, s)) v
      | _ -> false, v )
    | Access (Variable (n, _), m, s), t -> (
      match List.find_opt (fun (m, _) -> n = m) (Helpers.t_get g) with
      | Some (_, t1) -> subsume g (Access (t1, m, s)) t v
      | _ -> false, v )
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
    | Forall (a1, k1, t1, s1), Forall (a2, _ (*k2*), t2, s2) ->
      (* Relation between k1 et k2 ? *)
      let n, v = Variables.fresh v in
      let g = Helpers.k_set [ n, k1 ] + g in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    (* Exists *)
    | Exists (a1, k1, t1, s1), Exists (a2, _ (*k2*), t2, s2) ->
      (* Relation between k1 et k2 *)
      let n, v = Variables.fresh v in
      let g = Helpers.k_set [ n, k1 ] + g in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      subsume g t1 t2 v
    (* Constructor *)
    | Const (n1, l1, _), Const (n2, l2, _) when n1 = n2 ->
      let b =
        List.for_all
          (fun (n, t2) ->
            Option.fold ~none:false
              ~some:(fun (_, t1) -> fst (subsume g t1 t2 v))
              (List.find_opt (fun (m, _) -> n = m) l1))
          l2
      in
      b, v
    (* Trait *)
    | Trait (_, _), Trait (_, _) -> false, v
    | _ -> false, v

  module Operator = struct
    let ( <:?> ) t1 t2 g = check g t1 t2

    let ( <? ) t1 t2 c g = subsume g t1 t2 c

    let ( |- ) g f = f g
  end
end
