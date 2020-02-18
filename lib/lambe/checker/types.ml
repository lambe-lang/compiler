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
