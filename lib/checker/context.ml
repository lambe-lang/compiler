type t = int * int

module Runtime = struct
  let incr (v, d) = v, d + 1

  let depth t = snd t
end

module Variables = struct
  let create = 0, 0

  let fresh (v, d) = "'a" ^ string_of_int v, (v + 1, d)

  let incr (v, d) = v, d + 1

  let depth t = snd t
end

type 'a state = t -> 'a * t
