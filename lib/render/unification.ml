open Lambe_checker.Unification

let pp ppf = function
  | CyclicUnification (t1, t2) ->
    Format.fprintf ppf "Cyclic unification %a@ and %a" Type.pp t1 Type.pp t2
  | CannotUnify (t1, t2) ->
    Format.fprintf ppf "Cannot unify %a@ and %a" Type.pp t1 Type.pp t2
