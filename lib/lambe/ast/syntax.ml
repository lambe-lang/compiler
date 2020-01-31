module Type = struct
  module Native = struct
    type t =
      | Int
      | String
      | Char
  end

  type t =
    | Native of Native.t
    | Ident of string
    | Arrow of t * t
    | Apply of t * t
    | Type
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
    | Ident of string
    | Abstraction of string * t
    | Apply of t * t
    (* Let constructions *)
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
        string * (string * Type.t) list * Type.t list * Type.t option * t list
    (* Type expression *)
    | Type of Type.t * Type.t list
    | Data of Type.t * (string * Type.t) list
    (* Function expression *)
    | Sig of string * Type.t * Type.t option * Type.t list
    | Def of string * Term.t
end
