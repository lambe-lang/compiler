val free_vars : Lambe_ast.Type.t -> string list

val substitute :
  (string * Lambe_ast.Type.t) list -> Lambe_ast.Type.t -> Lambe_ast.Type.t
