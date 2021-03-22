open Lambe_ast

module Render : sig
  val pp : Format.formatter -> 'a Expr.t -> unit
end
