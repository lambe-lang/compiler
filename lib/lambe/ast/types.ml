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
