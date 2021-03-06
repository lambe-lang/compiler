module Checker : sig
  val check :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Kind.t
    -> bool

  val reduce :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t -> 'a Lambe_ast.Type.t option

  val subsume :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Type.t
    -> bool Context.state

  val upper_type :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Type.t
    -> 'a Lambe_ast.Type.t Option.t Context.state

  module Operator : sig
    val ( <:?> ) :
         'a Lambe_ast.Type.t
      -> 'a Lambe_ast.Kind.t
      -> 'a Lambe_ast.Type.gamma
      -> bool

    val ( <? ) :
         'a Lambe_ast.Type.t
      -> 'a Lambe_ast.Type.t
      -> Context.t
      -> 'a Lambe_ast.Type.gamma
      -> bool * Context.t

    val ( |- ) :
      'a Lambe_ast.Type.gamma -> ('a Lambe_ast.Type.gamma -> 'b) -> 'b

    val ( --> ) :
         'a Lambe_ast.Type.t
      -> 'a Lambe_ast.Type.t option
      -> 'a Lambe_ast.Type.gamma
      -> bool
  end
end
