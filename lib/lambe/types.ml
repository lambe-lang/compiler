module Type = struct
    module Native = struct
        type t =
        | Int
        | String
        | Char
    end

    type t =
    (* Native expressions *)
    | Native of Native.t
    | Ident of string
    | Abstraction of string * t * t
    | Apply of t * t
end
