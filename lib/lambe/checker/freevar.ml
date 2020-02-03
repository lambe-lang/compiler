open Lambe_ast.Ast.Type

let rec from =
  let remove n l =
    List.fold_right (fun e r -> if e = n then r else e :: r) l []
  in
  function
  | Variable v -> [ v ]
  | Arrow (t1, t2) -> from t1 @ from t2
  | Apply (t1, t2) -> from t1 @ from t2
  | Forall (n, _, t2) -> remove n @@ from t2
  | _ -> []
