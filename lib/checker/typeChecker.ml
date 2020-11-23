(*
    Lambë Type checker
*)

open Lambe_ast

let synthetize_literal = function
  | Term.(Integer _) -> "int"
  | Term.(Float _) -> "float"
  | Term.(String _) -> "string"
  | Term.(Char _) -> "char"

let check _gamma _e _t = true

and synthetize _gamma = function
  | Term.(Literal l) -> Result.Ok (Type.Variable (synthetize_literal l))
  | _ -> Result.Error "type error"
