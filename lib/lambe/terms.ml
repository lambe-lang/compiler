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
    (* Trait expression *)
    | Impl of Type.t * Type.t list * Type.t option * Type.t list
    | Trait of Type.t * Type.t list * Type.t option * Type.t list
    (* Type expression *)
    | Type of Type.t * Type.t list
    | Data of Type.t * (string * Type.t) list
    (* Function expression *)
    | Sig of string * Type.t * Type.t option * Type.t list
    | Def of string * t
end

(**
sig (++) : self -> self -> self for List a

Term.Sig("++",                                                                                  -- name
         Type.Abstraction("_",Ident("self),Type.Abstraction("_",Ident("self),Ident("self"))),   -- type
         Some(Type.Apply(Type.Ident("List"),Type.Ident("a"))),                                  -- self
         []                                                                                     -- with
        )

data Nil

Term.Data(Type.Ident("Nil"), [])

**)
