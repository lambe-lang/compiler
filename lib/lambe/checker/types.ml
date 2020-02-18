open Lambe_ast.Type

module Freevars = struct
  let add e env = if List.exists (( = ) e) env then env else e :: env

  let remove e = List.fold_left (fun r v -> if e = v then r else v :: r) []

  let free_vars =
    let rec from unbound = function
      | Variable v -> add v unbound
      | Arrow (t1, t2) -> from (from unbound t2) t1
      | Apply (t1, t2) -> from (from unbound t2) t1
      | Forall (n, _, t2) -> remove n @@ from unbound t2
      | _ -> unbound
    in
    from []
end

let free_vars = Freevars.free_vars

module Substitution = struct
  let substitute =
    let rec substitute n v = function
      | Variable m -> if n = m then v else Variable m
      | Arrow (t1, t2) -> Arrow (substitute n v t1, substitute n v t2)
      | Apply (t1, t2) -> Apply (substitute n v t1, substitute n v t2)
      | Forall (m, k, t2) ->
        Forall (m, k, if n = m then t2 else substitute n v t2)
      | t -> t
    in
    List.fold_right (fun (n, v) t -> substitute n v t)
end

let substitute = Substitution.substitute

type unification_error =
  | CyclicUnification of t * t
  | CannotUnify of t * t

module Unification = struct
  (* Naive implementation for the moment *)
  let unify t1 t2 =
    let ( >>= ) = Result.bind in
    let rec unify t1 t2 s =
      match t1, t2 with
      | t1, t2 when t1 = t2 -> Ok s
      | Arrow (t11, t12), Arrow (t21, t22) ->
        unify t12 t22 s
        >>= (fun s -> unify (substitute s t11) (substitute s t21) s)
      | Apply (t11, t12), Apply (t21, t22) ->
        unify t12 t22 s
        >>= (fun s -> unify (substitute s t11) (substitute s t21) s)
      | Variable n, t | t, Variable n ->
        if List.mem n @@ free_vars t
        then Error (CyclicUnification (t1, t2))
        else Ok ((n, t) :: s)
      | Forall (n1, k1, t1), Forall (n2, k2, t2) when k1 = k2 ->
        unify t1 (substitute [ n1, Variable n2 ] t2) ((n1, Variable n2) :: s)
      | _ -> Error (CannotUnify (t1, t2))
    in
    unify t1 t2 []
end

let unify = Unification.unify
