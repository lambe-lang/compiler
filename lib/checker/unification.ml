open Lambe_ast.Type
open Variables
open Substitution

type unification_error =
  | CyclicUnification of Lambe_ast.Type.t * Lambe_ast.Type.t
  | CannotUnify of Lambe_ast.Type.t * Lambe_ast.Type.t

type substitutions = (string * Lambe_ast.Type.t) list

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
