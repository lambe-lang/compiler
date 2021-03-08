(*

Type and environment definition

τ =
  | α
  | m
  | τ → τ
  | τ􏰀 #> τ
  | τ τ
  | τ + τ
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

type 'a set = (string * 'a) list

type 'a t =
  | Variable of string * 'a
  | Arrow of 'a t * 'a t * 'a
  | Invoke of 'a t * 'a t * 'a
  | Apply of 'a t * 'a t * 'a
  | Sum of 'a t * 'a t * 'a
  | Forall of string * 'a Kind.t * 'a t * 'a
  | Exists of string * 'a Kind.t * 'a t * 'a
  | Rec of string * 'a t * 'a
  | Const of string * (string * 'a t) list * 'a
  | Trait of 'a gamma * 'a

and 'a gamma = Gamma of 'a Kind.t set * 'a t set * 'a t set * 'a gamma list

(*
   The polymorphic type 'a holds open informations
   like locations etc. depending on the context
 *)
