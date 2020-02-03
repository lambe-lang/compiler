module S : sig
  type t =
    (* Trait expression *)
    | Impl of Type.S.t list * Type.S.t option * t list
    | Trait of
        string
        * (string * Kind.S.t) list
        * Type.S.t list
        * Type.S.t option
        * t list
    (* Type expression *)
    | Type of Type.S.t * Type.S.t list
    | Data of Type.S.t * (string * Type.S.t) list
    (* Function expression *)
    | Sig of string * Type.S.t * Type.S.t option * Type.S.t list
    | Def of string * Term.S.t
end
