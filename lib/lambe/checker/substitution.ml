open Lambe_ast.Type

let substitute =
  let rec substitute n v = function
    | Variable m -> if n = m then v else Variable m
    | Arrow (t1, t2) -> Arrow (substitute n v t1, substitute n v t2)
    | Apply (t1, t2) -> Apply (substitute n v t1, substitute n v t2)
    | Forall (m, k, t2) -> Forall (m, k, if n = m then t2 else substitute n v t2)
    | t -> t
  in
  List.fold_right (fun (n, v) t -> substitute n v t)
