open Lambe_ast

module Render : sig
  val check : Format.formatter -> 'a Type.t -> 'a Kind.t -> unit

  val subtype : Format.formatter -> 'a Type.t -> 'a Type.t -> unit

  val pp : Format.formatter -> 'a Type.t -> unit
end
