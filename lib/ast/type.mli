type t =
  | Path of string list (* ??? *)
  | Variable of string
  | Apply of t * t
  | Forall of string * Kind.t * t
