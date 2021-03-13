open Lambe_ast

module Gamma = struct
  let star = Kind.Type ()

  let ( |-> ) k k' = Kind.Arrow (k, k', ())

  let trait l = Kind.Trait (l, ())
end

module Types = struct
  let v n = Type.Variable (n, ())

  let gamma k t s w = Type.Gamma (k, t, s, w)

  let ( |-> ) t t' = Type.Arrow (t, t', ())

  let ( |@> ) t t' = Type.Invoke (t, t', ())

  let ( % ) t t' = Type.Apply (t, t', ())

  let ( <|> ) t t' = Type.Union (t, t', ())

  let ( @ ) t n = Type.Access (t, n, ())

  let forall (n, k) t = Type.Forall (n, k, t, ())

  let exists (n, k) t = Type.Exists (n, k, t, ())

  let mu n t = Type.Rec (n, t, ())

  let data c l = Type.Const (c, l, ())

  let trait k t s w = Type.Trait (gamma k t s w, ())
end
