open Lambe_ast.Ast.Type
module Constraints = Constraints.S

let deref t constraints =
  let deref_by_name n =
    match Constraints.find n constraints with
    | None -> t
    | Some v -> v
  in
  match t with
  | Variable n -> deref_by_name n
  | _ -> t

let unify t1 t2 constraints =
  let rec unify t1 t2 constraints =
    if t1 = t2
    then constraints
    else
      match t1, t2 with
      | Arrow (t11, t12), Arrow (t21, t22) ->
          unify t11 t21 @@ unify t12 t22 constraints
      | Apply (t11, t12), Apply (t21, t22) ->
          unify t11 t21 @@ unify t12 t22 constraints
      | Variable s, _ ->
          if List.mem s @@ Freevar.from t2
          then failwith "Cyclic unification"
          else Constraints.add s t2 constraints
      | _, Variable _ -> unify t2 t1 constraints
      | _, _ -> failwith "Unification fails"
  in
  unify (deref t1 constraints) (deref t2 constraints) constraints
