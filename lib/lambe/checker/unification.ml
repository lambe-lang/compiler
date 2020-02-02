let unify env t1 t2 =
  let open Lambe_ast.Syntax.Type in
  let rec unify env t1 t2 =
    if t1 = t2
    then env
    else
      match t1, t2 with
      | Arrow (t11, t12), Arrow (t21, t22) -> unify (unify env t11 t21) t12 t22
      | Apply (t11, t12), Apply (t21, t22) -> unify (unify env t11 t21) t12 t22
      | Variable s, _ ->
          let free_var = Freevar.from t2 in
          if List.mem s free_var
          then failwith "Cyclic unification"
          else Environment.add s t2 env
      | _, Variable _ -> unify env t2 t1
      | _, _ -> failwith "Unification fails"
  in
  unify env (Environment.deref env t1) (Environment.deref env t2)
