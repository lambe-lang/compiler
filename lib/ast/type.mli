type t =
  | Variable of string
  | Apply of t * t
  | Forall of string * Kind.t * t
  (* Located type *)
  | Located of t * Location.t
