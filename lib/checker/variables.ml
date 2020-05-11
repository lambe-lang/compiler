open Lambe_ast.Type

let free_vars =
  let add e env = if List.exists (( = ) e) env then env else e :: env in
  let remove e = List.fold_left (fun r v -> if e = v then r else v :: r) [] in
  let rec from unbound = function
    | Path _ -> unbound
    | Variable v -> add v unbound
    | Apply (t1, t2) -> from (from unbound t2) t1
    | Forall (n, _, t2) -> remove n @@ from unbound t2
  in
  from []
