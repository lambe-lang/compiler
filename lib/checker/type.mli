module Checker : sig
  val check :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Kind.t
    -> bool

  val synthetize :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t -> 'a Lambe_ast.Kind.t option

  val reduce :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t -> 'a Lambe_ast.Type.t option

  val subsume :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Type.t
    -> bool Context.state

  module Operator : sig
    val ( <:?> ) :
         'a Lambe_ast.Type.t
      -> 'a Lambe_ast.Kind.t
      -> 'a Lambe_ast.Type.gamma
      -> bool

    val ( <? ) :
         'a Lambe_ast.Type.t
      -> 'a Lambe_ast.Type.t
      -> Context.Variables.t
      -> 'a Lambe_ast.Type.gamma
      -> bool * Context.Variables.t

    val ( |- ) :
      'a Lambe_ast.Type.gamma -> ('a Lambe_ast.Type.gamma -> 'b) -> 'b
  end
end
