(*

ε= α
| m
| λα.ε
| ε ε
| let α = ε in ε
| let use ε in ε
| when(α).{τi -> εi}I
| Σ
| ε.m
| {τ,ε}
| let {τ,α} = ε in ε


Σ=Γ􏰂 * M

*)

type 'a t =
  | Variable of Common.var * 'a
  | Lambda of Common.var * 'a t * 'a
  | Apply of 'a t * 'a t * 'a
  | Bind of Common.var * 'a t * 'a t * 'a
  | Use of 'a t * 'a t * 'a
  | Trait of 'a Type.gamma * 'a t Common.dictionary * 'a
  | Access of 'a t * Common.id * 'a
  | When of Common.var * ('a Type.t * 'a t) list * 'a
  | Pack of 'a Type.t * 'a t * 'a
  | Unpack of 'a Type.t * Common.var * 'a t * 'a t * 'a
