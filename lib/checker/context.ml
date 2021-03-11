module Variables = struct
  type t = int

  let create = 0

  let fresh i = "'a" ^ string_of_int i, i + 1
end
