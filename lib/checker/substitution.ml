open Lambe_ast.Type

let substitute =
  let rec substitute n v = function
    | Path p -> Path p
    | Variable m -> if n = m then v else Variable m
    | Apply (t1, t2) -> Apply (substitute n v t1, substitute n v t2)
    | Forall (m, k, t2) ->
      let t2' =
        if n = m then Forall (n, k, substitute n v t2) else Forall (m, k, substitute n v t2)
      in
      Forall (m, k, t2')
  in
  List.fold_right (fun (n, v) t -> substitute n v t)
