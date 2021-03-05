(*
    Lambë Subtype checker
*)

open Lambe_ast

val check : 'a -> Term.t -> Type.t -> bool
