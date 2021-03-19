module Variables : sig
  type t

  val create : t

  val fresh : t -> string * t
end

type 'a state = Variables.t -> 'a * Variables.t
