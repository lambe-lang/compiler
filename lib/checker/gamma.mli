module Monoid : sig
  val neutral : 'a Lambe_ast.Type.gamma

  val combine :
       'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.gamma
    -> 'a Lambe_ast.Type.gamma
end

module Helpers : sig
  val k_get :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Kind.t Lambe_ast.Common.dictionary

  val k_set :
    'a Lambe_ast.Kind.t Lambe_ast.Common.dictionary -> 'a Lambe_ast.Type.gamma

  val t_get :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t Lambe_ast.Common.dictionary

  val t_set :
    'a Lambe_ast.Type.t Lambe_ast.Common.dictionary -> 'a Lambe_ast.Type.gamma

  val s_get :
    'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.t Lambe_ast.Common.dictionary

  val s_set :
    'a Lambe_ast.Type.t Lambe_ast.Common.dictionary -> 'a Lambe_ast.Type.gamma

  val w_get : 'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.gamma list

  val w_set : 'a Lambe_ast.Type.gamma list -> 'a Lambe_ast.Type.gamma
end

val empty : 'a Lambe_ast.Type.gamma

val merge :
  'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.gamma

val ( + ) :
  'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.gamma -> 'a Lambe_ast.Type.gamma

val ( <? ) :
     'a Lambe_ast.Common.dictionary
  -> 'a Lambe_ast.Common.dictionary
  -> ('a -> 'a -> bool)
  -> bool
