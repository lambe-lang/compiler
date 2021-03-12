open Lambe_ast

module Render : sig
  val subtype : Format.formatter -> 'a Type.t -> 'a Type.t -> unit

  val pp : Format.formatter -> 'a Type.t -> unit
end
