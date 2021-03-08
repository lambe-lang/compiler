(*

κ =
  | ⋆
  | κ→κ

*)

type 'a t =
  | Type of 'a
  | Arrow of 'a t * 'a t * 'a

(*
   The polymorphic type 'a holds open informations
   like locations etc. depending on the context
 *)
