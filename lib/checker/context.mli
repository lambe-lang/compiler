module Variables : sig
  type t

  val create : t

  val fresh : t -> string * t
end
