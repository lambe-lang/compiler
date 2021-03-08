open Lambe_ast

module Checker : sig
  val subsume : 'a Kind.t -> 'a Kind.t -> bool
end
