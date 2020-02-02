module Kind = struct
  type t =
    | Arrow of t * t
    | Type
end

module Type = struct
  module Native = struct
    type t =
      | Int
      | String
      | Char
  end

  type t =
    | Native of Native.t
    | Variable of string
    | Ident of string
    | Apply of t * t
    | Arrow of t * t
    | Forall of string * Kind.t * t
end

module Term = struct
  module Native = struct
    type t =
      | Int of int
      | String of string
      | Char of char
  end

  type t =
    (* Native expressions *)
    | Native of Native.t
    (* Lambda expression *)
    | Variable of string
    | Abstraction of string * t
    | Apply of t * t
    (* Let constructions *)
    | Ident of string
    | Let of string * t * t
    | LetImpl of Type.t list * Type.t option * t list * t
    (* Smart cast *)
    | When of string * t * (Type.t * t) list
end

module Entity = struct
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
end
