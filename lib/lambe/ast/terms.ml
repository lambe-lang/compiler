include Option
include Types

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
    | Abstraction of string * Type.t option * t
    | Apply of t * t
    | Let of string * t * t
    (* Trait expression *)
    | Impl of Type.t list * Type.t option * t list
    | Trait of
        string * (string * Type.t) list * Type.t list * Type.t option * t list
    (* Type expression *)
    | Type of Type.t * Type.t list
    | Data of Type.t * (string * Type.t) list
    (* Function expression *)
    | Sig of string * Type.t * Type.t option * Type.t list
    | Def of string * t
end
