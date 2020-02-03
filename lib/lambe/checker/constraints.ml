module S = struct
  module Gamma = Map.Make (String)

  type t = Lambe_ast.Ast.Type.t Gamma.t

  let empty = Gamma.empty

  let find = Gamma.find_opt

  let add = Gamma.add
end
