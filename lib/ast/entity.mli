type tparams = (string * Kind.t) list

type with_types = Type.t list

type for_type = Type.t option

type t =
  (* Trait expression *)
  | Impl of tparams * Type.t * for_type * with_types * t list
  | Trait of string * tparams * for_type * with_types * t list
  (* Kind expression *)
  | Kind of string * Kind.t
  (* Type expression *)
  | Type of string * tparams * Type.t
  | Enum of string * tparams * Type.t list
  | Data of string * tparams * (string * Type.t) list
  (* Function type & expression *)
  | Sig of string * Type.t * for_type * with_types
  | Def of string * Term.t
