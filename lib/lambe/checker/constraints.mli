open Lambe_ast

type t

val deref : t -> Type.t -> Type.t

val add : string -> Type.t -> t -> t

val create : unit -> t
