(*

Type and environment definition

τ =
  | α
  | m
  | τ → τ
  | τ􏰀 @> τ
  | τ τ
  | τ + τ
  | Λ(α : κ).τ
  | ∀(α : κ).τ
  | ∃(α : κ).τ
  | μ(α).τ
  | c S
  | Γ
  | τ.m

K = {mi:κi}I
T = {mi􏰁=τi}I
S = {mi:τi}I
W = {Γi}I
M = {mi􏰁εi}I
Γ = ⟨K,T,S,W⟩

*)

type 'a t =
  | Variable of Common.var * 'a
  | Arrow of 'a t * 'a t * 'a
  | Invoke of 'a t * 'a t * 'a
  | Union of 'a t * 'a t * 'a
  | Apply of 'a t * 'a t * 'a
  | Lambda of Common.var * 'a Kind.t * 'a t * 'a
  | Forall of Common.var * 'a Kind.t * 'a t * 'a
  | Exists of Common.var * 'a Kind.t * 'a t * 'a
  | Rec of Common.var * 'a t * 'a
  | Const of Common.id * 'a t Common.dictionary * 'a
  | Trait of 'a gamma * 'a
  | Access of 'a t * Common.id * 'a

and 'a gamma =
  | Gamma of
      'a Kind.t Common.dictionary
      * 'a t Common.dictionary
      * 'a t Common.dictionary
      * 'a gamma list

(*
   The polymorphic type 'a holds open informations
   like locations etc. depending on the context
 *)
