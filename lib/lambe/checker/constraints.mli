module S : sig
  open Lambe_ast.Ast

  type t

  val empty : t

  val find : string -> t -> Type.t option

  val add : string -> Type.t -> t -> t
end
