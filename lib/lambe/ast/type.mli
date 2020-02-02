module Native : sig
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
