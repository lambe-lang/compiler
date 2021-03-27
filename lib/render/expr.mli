open Lambe_ast

module Render : sig
  val pp : Format.formatter -> 'a Expr.t -> unit

  val check : Format.formatter -> 'a Expr.t -> 'a Type.t option -> unit
end
