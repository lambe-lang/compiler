module S : sig
  module Native : sig
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
    | LetImpl of Type.S.t list * Type.S.t option * t list * t
    (* Smart cast *)
    | When of string * t * (Type.S.t * t) list
end
