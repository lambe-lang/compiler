module Checker : sig
  val check :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Expr.t
    -> 'a Lambe_ast.Type.t
    -> bool Context.state

  module Operator : sig
    val ( <:?> ) :
         'a Lambe_ast.Expr.t
      -> 'a Lambe_ast.Type.t
      -> Context.Variables.t
      -> 'a Lambe_ast.Type.gamma
      -> bool * Context.Variables.t

    val ( |- ) :
      'a Lambe_ast.Type.gamma -> ('a Lambe_ast.Type.gamma -> 'b) -> 'b
  end
end
