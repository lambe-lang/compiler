val free_vars : Lambe_ast.Type.t -> string list

val substitute :
  (string * Lambe_ast.Type.t) list -> Lambe_ast.Type.t -> Lambe_ast.Type.t

type unification_error =
  | CyclicUnification of Lambe_ast.Type.t * Lambe_ast.Type.t
  | CannotUnify of Lambe_ast.Type.t * Lambe_ast.Type.t

val unify :
     Lambe_ast.Type.t
  -> Lambe_ast.Type.t
  -> ((string * Lambe_ast.Type.t) list, unification_error) result
