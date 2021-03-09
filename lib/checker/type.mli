open Lambe_ast

module Checker : sig
  type 'a state = Variable.t -> bool * Variable.t

  val subsume :
    'a Type.gamma -> 'a Type.t -> 'a Type.t -> Variable.t -> bool * Variable.t
end
