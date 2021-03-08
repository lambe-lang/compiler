open Lambe_ast

module Checker : sig
  val subsume : 'a Type.gamma -> 'a Type.t -> 'a Type.t -> bool
end
