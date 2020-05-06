type unification_error =
  | CyclicUnification of Lambe_ast.Type.t * Lambe_ast.Type.t
  | CannotUnify of Lambe_ast.Type.t * Lambe_ast.Type.t

type substitutions = (string * Lambe_ast.Type.t) list

val unify :
     Lambe_ast.Type.t
  -> Lambe_ast.Type.t
  -> (substitutions, unification_error) result
