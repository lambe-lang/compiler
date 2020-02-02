module Gamma = Map.Make (String)

type t = C of Lambe_ast.Type.t Gamma.t

let create () = C Gamma.empty

let deref (C env) t =
  let open Lambe_ast.Type in
  let deref_by_name n =
    match Gamma.find_opt n env with
    | None -> t
    | Some v -> v
  in
  match t with
  | Variable n -> deref_by_name n
  | _ -> t

let add e t (C env) = C (Gamma.add e t env)
