(*
    Lambë Type checker
*)

open Lambe_ast

val check : 'a -> Term.t -> Type.t -> bool

val synthetize : 'a -> Term.t -> (Type.t, string) result
