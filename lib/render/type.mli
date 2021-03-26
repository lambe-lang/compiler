open Lambe_ast

module Render : sig
  val check : Format.formatter -> 'a Type.t -> 'a Kind.t -> unit

  val reduce : Format.formatter -> 'a Type.t -> 'a Type.t option -> unit

  val subtype : Format.formatter -> 'a Type.t -> 'a Type.t -> unit

  val pp : Format.formatter -> 'a Type.t -> unit

  val pp_gamma : Format.formatter -> 'a Type.gamma -> unit
end
