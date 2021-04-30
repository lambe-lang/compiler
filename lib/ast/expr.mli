(*

ε= α
| λα.ε
| ζ.ε
| ε ε
| let α = ε in ε
| let α:τ = ε in ε
| when(α).{τi -> εi}I
| Σ
| ε.ε
| unpack {α,α} = ε in ε
| pack {τ,ε}
| ε as τ


Σ=Γ*M

*)

type 'a t =
  | Variable of Common.var * 'a
  | Lambda of Common.var * 'a t * 'a
  | Method of 'a t * 'a
  | Apply of 'a t * 'a t * 'a
  | Bind of Common.var * 'a t * 'a t * 'a
  | Use of 'a t * 'a t * 'a
  | Trait of 'a Type.gamma * 'a t Common.dictionary * 'a
  | When of Common.var * ('a Type.t * 'a t) list * 'a
  | Pack of 'a Type.t * 'a t * (Common.var * 'a Kind.t * 'a Type.t) * 'a
  | Unpack of Common.var * Common.var * 'a t * 'a t * 'a
  | As of 'a t * 'a Type.t * 'a
