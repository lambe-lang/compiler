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
      -> Context.t
      -> 'a Lambe_ast.Type.gamma
      -> bool * Context.t

    val ( |- ) :
      'a Lambe_ast.Type.gamma -> ('a Lambe_ast.Type.gamma -> 'b) -> 'b
  end
end
