open Lambe_ast

module Render : sig
  val pp : Format.formatter -> 'a Kind.t -> unit
end
