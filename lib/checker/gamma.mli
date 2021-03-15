open Lambe_ast

module Monoid : sig
  val neutral : 'a Type.gamma

  val combine : 'a Type.gamma -> 'a Type.gamma -> 'a Type.gamma
end

module Helpers : sig
  val k_get : 'a Type.gamma -> 'a Kind.t Type.dictionary

  val k_set : 'a Kind.t Type.dictionary -> 'a Type.gamma

  val t_get : 'a Type.gamma -> 'a Type.t Type.dictionary

  val t_set : 'a Type.t Type.dictionary -> 'a Type.gamma

  val s_get : 'a Type.gamma -> 'a Type.t Type.dictionary

  val w_get : 'a Type.gamma -> 'a Type.gamma list
end

val empty : 'a Type.gamma

val merge : 'a Type.gamma -> 'a Type.gamma -> 'a Type.gamma

val ( + ) : 'a Type.gamma -> 'a Type.gamma -> 'a Type.gamma

val ( <? ) :
  'a Type.dictionary -> 'a Type.dictionary -> ('a -> 'a -> bool) -> bool
