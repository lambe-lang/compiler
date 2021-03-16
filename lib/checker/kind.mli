module Checker : sig
  val subsume : 'a Lambe_ast.Kind.t -> 'a Lambe_ast.Kind.t -> bool

  module Operator : sig
    val ( <? ) : 'a Lambe_ast.Kind.t -> 'a Lambe_ast.Kind.t -> bool
  end
end
