open Lambe_ast

module Kinds = struct
  let star = Kind.Type ()

  let ( |-> ) k k' = Kind.Arrow (k, k', ())

  let trait l = Kind.Trait (l, ())
end

module Types = struct
  let gamma k t s w = Type.Gamma (k, t, s, w)

  let v n = Type.Variable (n, ())

  let ( |-> ) t t' = Type.Arrow (t, t', ())

  let ( |=> ) t t' = Type.Invoke (t, t', ())

  let ( <$> ) t t' = Type.Apply (t, t', ())

  let ( <|> ) t t' = Type.Union (t, t', ())

  let lambda (n, k) t = Type.Lambda (n, k, t, ())

  let forall (n, k) t = Type.Forall (n, k, t, ())

  let exists (n, k) t = Type.Exists (n, k, t, ())

  let mu (n, k) t = Type.Rec (n, k, t, ())

  let data c l = Type.Const (c, l, ())

  let trait k t s w = Type.Trait (gamma k t s w, ())

  let ( @ ) t t' = Type.Use (t, t', ())
end

module Exprs = struct
  let v n = Expr.Variable (n, ())

  let lambda v e = Expr.Lambda (v, e, ())

  let zeta e = Expr.Method (e, ())

  let ( <$> ) e e' = Expr.Apply (e, e', ())

  let bind v e e' = Expr.Bind (v, None, e, e', ())

  let bindT v t e e' = Expr.Bind (v, Some t, e, e', ())

  let use e e' = Expr.Use (e, e', ())

  let switchType v l = Expr.When (v, l, ())

  let trait g l = Expr.Trait (g, l, ())

  let ( @ ) e e' = Expr.Use (e, e', ())

  let pack t e = Expr.Pack (t, e, ())

  let unpack t n e e' = Expr.Unpack (t, n, e, e', ())
end
