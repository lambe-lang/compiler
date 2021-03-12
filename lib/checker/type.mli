open Lambe_ast

module Checker : sig
  type 'a state = Context.Variables.t -> bool * Context.Variables.t

  val check : 'a Type.gamma -> 'a Type.t -> 'a Kind.t -> bool

  val subsume :
       'a Type.gamma
    -> 'a Type.t
    -> 'a Type.t
    -> Context.Variables.t
    -> bool * Context.Variables.t
end
