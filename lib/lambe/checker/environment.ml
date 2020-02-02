module Gamma = Map.Make (String)

let deref env t =
  let open Lambe_ast.Syntax.Type in
  match t with
  | Variable n -> (
      match Gamma.find_opt n env with
      | None -> t
      | Some v -> v)
  | _ -> t

let add e env = Gamma.add e env
