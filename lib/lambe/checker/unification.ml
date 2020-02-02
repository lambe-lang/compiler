let unify t1 t2 env =
  let open Lambe_ast.Type in
  let rec unify t1 t2 env =
    if t1 = t2
    then env
    else
      match t1, t2 with
      | Arrow (t11, t12), Arrow (t21, t22) -> unify t11 t21 @@ unify t12 t22 env
      | Apply (t11, t12), Apply (t21, t22) -> unify t11 t21 @@ unify t12 t22 env
      | Variable s, _ ->
          if List.mem s @@ Freevar.from t2
          then failwith "Cyclic unification"
          else Constraints.add s t2 env
      | _, Variable _ -> unify t2 t1 env
      | _, _ -> failwith "Unification fails"
  in
  unify (Constraints.deref env t1) (Constraints.deref env t2) env
