type t =
  (* Trait expression *)
  | Impl of Type.t list * Type.t option * t list
  | Trait of
      string * (string * Kind.t) list * Type.t list * Type.t option * t list
  (* Type expression *)
  | Type of Type.t * Type.t list
  | Data of Type.t * (string * Type.t) list
  (* Function expression *)
  | Sig of string * Type.t * Type.t option * Type.t list
  | Def of string * Term.t
