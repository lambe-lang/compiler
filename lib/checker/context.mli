type t

module Runtime : sig
  val incr : t -> t

  val depth : t -> int
end

module Variables : sig
  val create : t

  val fresh : t -> string * t

  val incr : t -> t

  val depth : t -> int
end

type 'a state = t -> 'a * t
