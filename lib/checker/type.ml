(*
    Used to extrace the context
*)

let ( <$> ) = Option.map

let ( >>= ) = Option.bind

module TypeContext = struct
  let get =
    let open Lambe_ast.Type in
    function
    | Variable (_, s) -> s
    | Arrow (_, _, s) -> s
    | Invoke (_, _, s) -> s
    | Apply (_, _, s) -> s
    | Union (_, _, s) -> s
    | Lambda (_, _, _, s) -> s
    | Forall (_, _, _, s) -> s
    | Exists (_, _, _, s) -> s
    | Rec (_, _, _, s) -> s
    | Const (_, _, s) -> s
    | Trait (_, s) -> s
    | Use (_, _, s) -> s
end

(*
    Provides basic functions used when a type defnition or kind definition
    should be retrieved from Gamma. This operation is performed with a
    depth first seach strategy.
*)
module Finder = struct
  let find_kind n g =
    let open Lambe_ast.Type in
    let open List in
    let rec find = function
      | [] -> None
      | Gamma (k, _, _, w) :: l -> (
        match find_opt (fun (m, _) -> n = m) k with
        | Some (_, k) -> Some k
        | None -> find (w @ l) )
    in
    find [ g ]

  let find_type n g =
    let open Lambe_ast.Type in
    let open List in
    let rec find = function
      | [] -> None
      | Gamma (_, t, _, w) :: l -> (
        match find_opt (fun (m, _) -> n = m) t with
        | Some (_, t) -> Some t
        | None -> find (w @ l) )
    in
    find [ g ]
end

module Distribute = struct
  let distribute g s =
    let open Lambe_ast.Type in
    function
    | Variable (_, _) as t -> Use (g, t, s)
    | Arrow (t1, t2, s') -> Arrow (Use (g, t1, s), Use (g, t2, s), s')
    | Invoke (t1, t2, s') -> Invoke (Use (g, t1, s), Use (g, t2, s), s')
    | Apply (t1, t2, s') -> Apply (Use (g, t1, s), Use (g, t2, s), s')
    | Union (t1, t2, s') -> Union (Use (g, t1, s), Use (g, t2, s), s')
    | Lambda (n, k, t, s') -> Lambda (n, k, Use (g, t, s), s')
    | Forall (n, k, t, s') -> Forall (n, k, Use (g, t, s), s')
    | Exists (n, k, t, s') -> Exists (n, k, Use (g, t, s), s')
    | Rec (n, k, t, s') -> Rec (n, k, Use (g, t, s), s')
    | t -> Use (g, t, s)
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
      | Union (t1, t2, s) -> Union (subs t1, subs t2, s)
      | Lambda (a, _, _, _) when a = v -> t
      | Lambda (a, k, t1, s) -> Lambda (a, k, subs t1, s)
      | Forall (a, _, _, _) when a = v -> t
      | Forall (a, k, t1, s) -> Forall (a, k, subs t1, s)
      | Exists (a, _, _, _) when a = v -> t
      | Exists (a, k, t1, s) -> Exists (a, k, subs t1, s)
      | Rec (a, _, _, _) when v = a -> t
      | Rec (a, k, t1, s) -> Rec (a, k, subs t1, s)
      | Const (a, l1, s) -> Const (a, map subst_field l1, s)
      | Trait (gamma, s) -> Trait (subst_gamma gamma, s)
      | Use (t1, t2, s) -> Use (subs t1, subs t2, s)
    in
    subs t
end

module Checker = struct
  let rec check g t k =
    let open Kind.Checker.Operator in
    let print_check = Lambe_render.Type.Render.check Format.std_formatter in
    let _ = print_check t k in
    Option.fold ~none:false ~some:(fun k' -> k' <? k) (synthetize g t)

  and synthetize g t =
    let module K = Lambe_ast.Kind in
    let open Lambe_ast.Type in
    let open Kind.Checker.Operator in
    let open List in
    match t with
    | Variable (n, _) -> Finder.find_kind n g
    | Arrow (_, _, s) -> Some (K.Type s)
    | Invoke (_, _, s) -> Some (K.Type s)
    | Apply (t1, t2, _) -> (
      synthetize g t1
      >>= function
      | K.Arrow (k', k, _) when check g t2 k' -> Some k | _ -> None )
    | Union (t1, t2, _) ->
      synthetize g t1
      >>= (fun k1 -> (fun k2 -> if k1 <? k2 then k2 else k1) <$> synthetize g t2)
    | Lambda (n, k, t, s) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      (fun k' -> K.Arrow (k, k', s)) <$> synthetize g t
    | Forall (n, k, t, _) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      synthetize g t
    | Exists (n, k, t, _) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      synthetize g t
    | Rec (n, k, t, _) ->
      let g = Gamma.(Helpers.k_set [ n, k ] + g) in
      synthetize g t
    | Const (_, s, l) ->
      if for_all (fun (_, t) -> check g t (K.Type l)) s
      then Some (K.Type l)
      else None
    | Trait ((Gamma (k, t, s, w) as g'), l) ->
      if for_all (fun (_, t) -> check Gamma.(g + g') t (K.Type l)) t
         && for_all (fun (_, t) -> check Gamma.(g + g') t (K.Type l)) s
         && for_all (fun g -> check Gamma.empty (Trait (g, l)) (K.Type l)) w
      then
        Some (K.Trait (fold_left (fun k (Gamma (k', _, _, _)) -> k @ k') k w, l))
      else None
    | Use (t1, t2, _) -> (
      synthetize g t1
      >>= function
      | Trait (l, _) -> synthetize Gamma.(Helpers.k_set l) t2 | _ -> None )

  let reduce g t =
    let open Substitution in
    let open Lambe_ast.Type in
    (* TODO(didier) first is Ugly -> remove it *)
    let rec reduce g t first =
      match t with
      | Variable (n, _) ->
        Some
          (Option.fold ~none:t ~some:Fun.id
             (Finder.find_type n g >>= (fun t -> reduce g t first)) )
      | Apply (t1, t2, _) -> (
        reduce g t1 false
        >>= function
        | Lambda (n, k, t1', _) when check g t2 k ->
          reduce g (substitute n t2 t1') first
        | _ -> None )
      | Use (t1, (Variable (_, _) as v), _) -> (
        reduce g t1 first
        >>= function
        | Trait (g', _) -> reduce g' v first >>= (fun t2 -> reduce g t2 first)
        | _ -> None )
      | Use (g, t, s) -> Some (Distribute.distribute g s t)
      | Rec (n, _, t', _) when first = false -> Some (substitute n t t')
      | _ -> Some t
    in
    reduce g t true

  let rec subsume g t1 t2 v =
    let open Context in
    let module K = Lambe_ast.Kind in
    let print_subtype = Lambe_render.Type.Render.subtype Format.std_formatter in
    let _ = print_string " Subsume > " in
    let _ = print_subtype t1 t2 in
    if t1 = t2
    then check g t1 (K.Type (TypeContext.get t1)), v
    else subsume_reduce_left g t1 t2 (Runtime.incr v)

  and subsume_reduce_left g t1 t2 v =
    match reduce g t1 with
    | None -> false, v
    | Some t1' when t1' != t1 -> subsume g t1' t2 v
    | _ -> subsume_reduce_right g t1 t2 v

  and subsume_reduce_right g t1 t2 v =
    match reduce g t2 with
    | None -> false, v
    | Some t2' when t2' != t2 -> subsume g t1 t2' v
    | Some _ -> subsume_arrow g t1 t2 v

  and subsume_arrow g t1 t2 v =
    let module K = Lambe_ast.Kind in
    let open Lambe_ast.Type in
    match t1, t2 with
    (* Arrow *)
    | Arrow (t1, t2, _), Arrow (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    | _ -> subsume_invoke g t1 t2 v

  and subsume_invoke g t1 t2 v =
    match t1, t2 with
    | Invoke (t1, t2, _), Invoke (t3, t4, _) ->
      let b1, v1 = subsume g t3 t1 v in
      if b1
      then
        let b2, v2 = subsume g t2 t4 v1 in
        b1 && b2, v2
      else false, v
    | _ -> subsume_union g t1 t2 v

  and subsume_union g t1 t2 v =
    match t1, t2 with
    | Union (t1, t2, _), t3 ->
      let b1, v1 = subsume g t1 t3 v in
      if b1 then subsume g t2 t3 v1 else false, v
    | t1, Union (t2, t3, _) ->
      let b1, v1 = subsume g t1 t2 v in
      if b1 then b1, v1 else subsume g t1 t3 v1
    | _ -> subsume_rec g t1 t2 v

  and subsume_rec g t1 t2 v =
    let open Context in
    let open Substitution in
    let open Kind.Checker.Operator in
    match t1, t2 with
    | Rec (a1, k1, t1, s1), Rec (a2, k2, t2, s2) ->
      if k1 <? k2
      then
        let n, v = Variables.fresh v in
        let t1 = substitute a1 (Variable (n, s1)) t1 in
        let t2 = substitute a2 (Variable (n, s2)) t2 in
        if fst (subsume g t1 t2 v) then true, v else subsume_rec_left g t1 t2 v
      else subsume_rec_left g t1 t2 v
    | _ -> subsume_rec_left g t1 t2 v

  and subsume_rec_left g t1 t2 v =
    let open Substitution in
    match t1, t2 with
    | Rec (a1, _, t1', _), _ ->
      if fst (subsume g (substitute a1 t1 t1') t2 v)
      then true, v
      else subsume_rec_right g t1 t2 v
    | _ -> subsume_rec_right g t1 t2 v

  and subsume_rec_right g t1 t2 v =
    let open Substitution in
    match t1, t2 with
    | _, Rec (a2, _, t2', _) ->
      if fst (subsume g t1 (substitute a2 t2 t2') v)
      then true, v
      else subsume_forall g t1 t2 v
    | _ -> subsume_forall g t1 t2 v

  and subsume_forall g t1 t2 v =
    let open Context in
    let open Substitution in
    let open Kind.Checker.Operator in
    match t1, t2 with
    | Forall (a1, k1, t1, s1), Forall (a2, k2, t2, s2) ->
      if k2 <? k1
      then
        let n, v = Variables.fresh v in
        let g = Gamma.(Helpers.k_set [ n, k2 ] + g) in
        let t1 = substitute a1 (Variable (n, s1)) t1 in
        let t2 = substitute a2 (Variable (n, s2)) t2 in
        subsume g t1 t2 v
      else false, v
    | _ -> subsume_exists g t1 t2 v

  and subsume_exists g t1 t2 v =
    let open Context in
    let open Substitution in
    let open Kind.Checker.Operator in
    match t1, t2 with
    | Exists (a1, k1, t1, s1), Exists (a2, k2, t2, s2) ->
      let n, v = Variables.fresh v in
      let g = Gamma.(Helpers.k_set [ n, k1 ] + g) in
      let t1 = substitute a1 (Variable (n, s1)) t1 in
      let t2 = substitute a2 (Variable (n, s2)) t2 in
      if k1 <? k2 then subsume g t1 t2 v else false, v
    | _ -> subsume_const g t1 t2 v

  and subsume_const g t1 t2 v =
    match t1, t2 with
    | Const (n1, l1, _), Const (n2, l2, _) when n1 = n2 ->
      Gamma.(l1 <? l2) (fun t1 t2 -> fst (subsume g t1 t2 v)), v
    (* Trait *)
    | _ -> subsume_trait g t1 t2 v

  and subsume_trait g t1 t2 v =
    let open Kind.Checker.Operator in
    match t1, t2 with
    | Trait (Gamma (k, t, s, _), l), Trait (Gamma (k', t', s', _), l') ->
      ( Trait (k, l) <? Trait (k', l')
        && Gamma.(t <? t') (fun t1 t2 -> fst (subsume g t1 t2 v))
        && Gamma.(s <? s') (fun t1 t2 -> fst (subsume g t1 t2 v))
      , v )
    | _ -> false, v

  let upper_type g t1 t2 v =
    if fst (subsume g t1 t2 v)
    then Some t2, v
    else if fst (subsume g t2 t1 v)
    then Some t1, v
    else None, v

  module Operator = struct
    let ( <:?> ) t1 t2 g = check g t1 t2

    let ( <? ) t1 t2 c g = subsume g t1 t2 c

    let ( --> ) t1 t2 g = reduce g t1 = t2

    let ( |- ) g f = f g
  end
end
