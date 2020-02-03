open Lambe_ast.Ast
module Constraints = Constraints.S

val unify : Type.t -> Type.t -> Constraints.t -> Constraints.t
