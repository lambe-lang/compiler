open Lambe_ast

module Monoid : sig
  val neutral : 'a Type.gamma

  val combine : 'a Type.gamma -> 'a Type.gamma -> 'a Type.gamma
end

module Helpers : sig
  val k_get : 'a Type.gamma -> 'a Kind.t Type.definitions

  val k_set : 'a Kind.t Type.definitions -> 'a Type.gamma

  val t_get : 'a Type.gamma -> 'a Type.t Type.definitions

  val t_set : 'a Type.t Type.definitions -> 'a Type.gamma

  val s_get : 'a Type.gamma -> 'a Type.t Type.definitions

  val w_get : 'a Type.gamma -> 'a Type.gamma list
end
