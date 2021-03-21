module Substitution = struct
  (* Substituste v by r in t *)
  let rec substitute v r t =
    let open Lambe_ast.Expr in
    let ( <$> ) = List.map in
    match t with
    | Variable (a, _) when v = a -> r
    | Variable _ -> t
    | Lambda (a, _, _) when a = v -> t
    | Lambda (a, e1, s) -> Lambda (a, substitute v r e1, s)
    | Method (e1, s) -> Method (substitute v r e1, s)
    | Apply (e1, e2, s) -> Apply (substitute v r e1, substitute v r e2, s)
    | Invoke (e1, e2, s) -> Invoke (substitute v r e1, substitute v r e2, s)
    | Bind (a, _, _, _) when v = a -> t
    | Bind (a, e1, e2, s) -> Bind (a, substitute v r e1, substitute v r e2, s)
    | Use (e1, e2, s) -> Use (substitute v r e1, substitute v r e2, s)
    | Trait (g, l, s) -> Trait (g, (fun (n, e) -> n, substitute v r e) <$> l, s)
    | Access (t1, n, s) -> Access (substitute v r t1, n, s)
    | When (n, l, s) -> When (n, (fun (t, e) -> t, substitute v r e) <$> l, s)
    | Pack (t, e, s) -> Pack (t, substitute v r e, s)
    | Unpack (t, n, e1, e2, s) ->
      Unpack (t, n, substitute v r e1, substitute v r e2, s)
end

module Checker = struct
  let ( <$> ) = Option.map

  let rec check g e t v =
    let open Context in
    let open Lambe_ast.Expr in
    let open Type.Checker.Operator in
    let module T = Lambe_ast.Type in
    let open List in
    (* let ( >>= ) ma f = Option.bind ma f in *)
    match e with
    | Lambda (a, e, s) -> (
      let n, v = Variables.fresh v in
      let e = Substitution.substitute a (Variable (n, s)) e in
      match Type.Checker.reduce g t with
      | Some T.(Arrow (t1, t2, _)) ->
        check Gamma.(Helpers.s_set [ n, t1 ] + g) e t2 v
      | _ -> false, v )
    | Method (e, _) -> (
      match Type.Checker.reduce g t with
      | Some T.(Invoke (t1, t2, _)) ->
        check Gamma.(Helpers.s_set [ "self", t1 ] + g) e t2 v
      | _ -> false, v )
    | _ ->
      let r, v = synthetize g e v in
      Option.fold ~none:(false, v) ~some:Fun.id
        ((fun t' -> g |- (t' <? t) v) <$> r)

  and synthetize g e v =
    let open List in
    let open Type.Checker.Operator in
    let module T = Lambe_ast.Type in
    match e with
    | Variable (n, _) ->
      snd <$> find_opt (fun (m, _) -> n = m) Gamma.Helpers.(s_get g), v
    | Apply (e1, e2, _) -> (
      match synthetize g e2 v with
      | Some T.(Invoke (t1, t2, _) as t3), v -> (
        let b, v = check g e2 t1 v in
        if b
        then Some t2, v
        else
          match synthetize g e1 v with
          | Some T.(Arrow (t1, t2, _)), v ->
            let b, v = g |- (t3 <? t1) v in
            if b then Some t2, v else None, v
          | _ -> None, v )
      | _ -> None, v )
    | Access (e, n, s) -> (
      match synthetize g e v with
      | Some T.(Trait (g', _)), v ->
        synthetize Gamma.(g' + g) (Variable (n, s)) v
      | Some T.(Const (_, l, _)), v ->
        synthetize Gamma.(Helpers.(s_set l) + g) (Variable (n, s)) v
      | _ -> None, v )
    | _ -> None, v

  module Operator = struct
    let ( <:?> ) t1 t2 s g = check g t1 t2 s

    let ( |- ) g f = f g
  end
end
