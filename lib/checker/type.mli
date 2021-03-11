open Lambe_ast

module Checker : sig
  type 'a state = Context.Variables.t -> bool * Context.Variables.t

  val subsume :
    'a Type.gamma -> 'a Type.t -> 'a Type.t -> Context.Variables.t -> bool * Context.Variables.t
end
