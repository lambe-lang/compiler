let ( <$> ) = Option.map

let ( <*> ) f a = (match f with Some f -> f <$> a | None -> None)

module Finder = struct
  let find_signature n g =
    let open Lambe_ast.Type in
    let open List in
    let rec find = function
      | [] -> None
      | Gamma (_, _, s, w) :: l -> (
        match find_opt (fun (m, _) -> n = m) s with
        | Some (_, t) -> Some t
        | None -> find (w @ l) )
    in
    find [ g ]
end

module Substitution = struct
  (* Substituste v by r in t *)
  let substitute v r t =
    let open Lambe_ast.Expr in
    let ( <$> ) = List.map in
    let rec subs = function
      | Variable (a, _) when v = a -> r
      | Variable _ -> t
      | Lambda (a, _, _) when a = v -> t
      | Lambda (a, e1, s) -> Lambda (a, subs e1, s)
      | Method (e1, s) -> Method (subs e1, s)
      | Apply (e1, e2, s) -> Apply (subs e1, subs e2, s)
      | Bind (a, _, _, _) when v = a -> t
      | Bind (a, e1, e2, s) -> Bind (a, subs e1, subs e2, s)
      | Use (e1, e2, s) -> Use (subs e1, subs e2, s)
      | Trait (g, l, s) -> Trait (g, (fun (n, e) -> n, subs e) <$> l, s)
      | When (n, l, s) -> When (n, (fun (t, e) -> t, subs e) <$> l, s)
      | Pack (t, e, t', s) -> Pack (t, subs e, t', s)
      | Unpack (t, n, e1, e2, s) -> Unpack (t, n, subs e1, subs e2, s)
      | HasType (e, t, s) -> HasType (subs e, t, s)
    in
    subs t
end

module Checker = struct
  let ( |- ) g f = f g

  let rec ( <:> ) e t s g = check g e t s

  and ( <:?> ) e s g = synthetize g e s

  and check g e t v =
    let module T = Lambe_ast.Type in
    let print_check = Lambe_render.Expr.Render.check Format.std_formatter in
    let _ = print_string " Checking > " in
    let _ = print_check e (Some t) in
    let result = check_has_type g e t v in
    let _ = print_string " Checking < " in
    let _ = print_check e (Some t) in
    result

  (** Private check functions **)

  and check_has_type g e t v =
    let module O = Type.Checker.Operator in
    match e with
    | HasType (e, t', _) ->
      let r, v' = g |- (e <:> t') v in
      if r then O.(g |- (t' <? t) v') else false, v
    | _ -> check_generic g e t v

  and check_generic g e t v =
    let open List in
    let module T = Lambe_ast.Type in
    match t with
    | T.Forall (n, k, t, _) -> check Gamma.(Helpers.k_set [ n, k ] + g) e t v
    | _ -> check_lambda g e t v

  and check_lambda g e t v =
    let open Context in
    let module T = Lambe_ast.Type in
    let open List in
    match e, t with
    | Lambda (a, e, s), _ -> (
      let n, v = Variables.fresh v in
      let e = Substitution.substitute a (Variable (n, s)) e in
      match Type.Checker.reduce g t with
      | Some T.(Arrow (t1, t2, _)) ->
        Gamma.(Helpers.s_set [ n, t1 ] + g) |- (e <:> t2) v
      | _ -> false, v )
    | _ -> check_method g e t v

  and check_method g e t v =
    let module T = Lambe_ast.Type in
    let open List in
    match e with
    | Method (e, _) -> (
      match Type.Checker.reduce g t with
      | Some T.(Invoke (t1, t2, _)) ->
        Gamma.(Helpers.s_set [ "self", t1 ] + g) |- (e <:> t2) v
      | _ -> false, v )
    | _ -> check_after_synthetize g e t v

  and check_after_synthetize g e t v =
    let module O = Type.Checker.Operator in
    let module T = Lambe_ast.Type in
    let r, v' = synthetize g e v in
    Option.fold ~none:(false, v) ~some:Fun.id
      O.((fun t' -> g |- (t' <? t) v') <$> r)

  and synthetize g e v =
    let print_check = Lambe_render.Expr.Render.check Format.std_formatter in
    let _ = print_string " > " in
    let _ = print_check e None in
    let result = synthetize_as_type g e v in
    let _ = print_string " < " in
    let _ = print_check e (fst result) in
    result

  (** Private synthetize functions **)

  and synthetize_as_type g e v =
    let open Lambe_ast.Expr in
    match e with
    | HasType (e, t, _) ->
      let r, v' = g |- (e <:> t) v in
      if r then Some t, v' else None, v
    | _ -> synthetize_variable g e v

  and synthetize_variable g e v =
    let open Lambe_ast.Expr in
    match e with
    | Variable (n, _) -> Finder.find_signature n g, v
    | _ -> synthetize_use g e v

  and synthetize_use g e v =
    let open Lambe_ast.Expr in
    let module T = Lambe_ast.Type in
    match e with
    | Use (e1, e2, _) -> (
      match g |- (e1 <:?> v) with
      | Some T.(Trait (g', _)), v -> Gamma.(g' + g) |- (e2 <:?> v)
      | Some T.(Const (_, l, _)), v ->
        Gamma.(Helpers.(s_set l) + g) |- (e2 <:?> v)
      | _ -> None, v )
    | _ -> synthetize_invoke g e v

  and synthetize_invoke g e v =
    let open Lambe_ast.Expr in
    let module T = Lambe_ast.Type in
    match e with
    | Apply (e1, e2, _) ->
      let t, v' = g |- (e2 <:?> v) in
      Option.fold ~none:(None, v) ~some:Fun.id
        ( (function
            | T.Invoke (t1, t2, _) ->
              let r, v = g |- (e1 <:> t1) v in
              if r then Some t2, v' else None, v
            | _ -> synthetize_apply g e v')
        <$> t )
    | _ -> synthetize_bind g e v

  and synthetize_apply g e v =
    let open Lambe_ast.Expr in
    let module T = Lambe_ast.Type in
    match e with
    | Apply (e1, e2, _) ->
      let t, v' = g |- (e1 <:?> v) in
      Option.fold ~none:(None, v) ~some:Fun.id
        ( (function
            | T.Arrow (t1, t2, _) ->
              let r, v' = g |- (e2 <:> t1) v' in
              if r then Some t2, v' else None, v'
            | _ -> None, v)
        <$> t )
    | _ -> synthetize_bind g e v

  and synthetize_bind g e v =
    let open Lambe_ast.Expr in
    let module T = Lambe_ast.Type in
    match e with
    | Bind (n, e1, e2, _) ->
      let r, v' = g |- (e1 <:?> v) in
      Option.fold ~none:(None, v)
        ~some:(fun t1 -> Gamma.(Helpers.s_set [ n, t1 ] + g) |- (e2 <:?> v'))
        r
    | _ -> synthetize_when g e v

  and synthetize_when g e v =
    let open Lambe_ast.Expr in
    let module T = Lambe_ast.Type in
    match e with
    | When (n, l, s) ->
      let tn, v' = g |- (Variable (n, s) <:?> v) in
      Option.fold ~none:(None, v)
        ~some:(fun t -> synthetize_cases g (n, s) t l v')
        tn
    | _ -> None, v

  and synthetize_cases g (n, s) t l v =
    let module O = Type.Checker.Operator in
    let module T = Lambe_ast.Type in
    match l with
    | [] -> None, v
    | [ (tc, e) ] ->
      if fst O.(g |- (tc <? t) v)
      then Gamma.(Helpers.s_set [ n, tc ] + g) |- (e <:?> v)
      else None, v
    | c :: l ->
      let rc, v' = synthetize_cases g (n, s) t [ c ] v in
      let rl, v'' =
        Option.fold ~none:(None, v)
          ~some:(fun _ -> synthetize_cases g (n, s) t l v')
          rc
      in
      ( (fun tl tr ->
          Option.fold
            ~none:(T.Union (tl, tr, s))
            ~some:Fun.id
            (fst (Type.Checker.upper_type g tl tr v'')))
        <$> rc
        <*> rl
      , v' )

  module Operator = struct
    let ( <:?> ) t1 t2 s g = check g t1 t2 s

    let ( |- ) g f = f g
  end
end
