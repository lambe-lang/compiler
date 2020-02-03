module S : sig
  type t =
    | Arrow of t * t
    | Type
end
