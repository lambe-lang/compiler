(*

κ =
  | ⋆
  | κ→κ
  | K

*)

type 'a t =
  | Type of 'a
  | Arrow of 'a t * 'a t * 'a
  | Trait of (string * 'a t) list * 'a

(*
   The polymorphic type 'a holds open informations
   like locations etc. depending on the context
 *)
