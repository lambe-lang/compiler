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

K = {mi:κi}I
T = {mi􏰁=τi}I
S = {mi:τi}I
W = {Γi}I
M = {mi􏰁εi}I
Γ = ⟨K,T,S,W⟩

*)

type 'a dictionary = (string * 'a) list

type id = string

type var = string

type 'a t =
  | Variable of var * 'a
  | Arrow of 'a t * 'a t * 'a
  | Invoke of 'a t * 'a t * 'a
  | Union of 'a t * 'a t * 'a
  | Apply of 'a t * 'a t * 'a
  | Lambda of var * 'a Kind.t * 'a t * 'a
  | Forall of var * 'a Kind.t * 'a t * 'a
  | Exists of var * 'a Kind.t * 'a t * 'a
  | Rec of var * 'a t * 'a
  | Const of id * 'a t dictionary * 'a
  | Trait of 'a gamma * 'a
  | Access of 'a t * id * 'a

and 'a gamma =
  | Gamma of
      'a Kind.t dictionary * 'a t dictionary * 'a t dictionary * 'a gamma list

(*
   The polymorphic type 'a holds open informations
   like locations etc. depending on the context
 *)
