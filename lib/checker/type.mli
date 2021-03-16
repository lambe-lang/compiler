module Checker : sig
  type 'a state = Context.Variables.t -> bool * Context.Variables.t

  val check :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Kind.t
    -> bool

  val synthetize :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t -> 'a Lambe_ast.Kind.t option

  val subsume :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Type.t
    -> Context.Variables.t
    -> bool * Context.Variables.t

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
