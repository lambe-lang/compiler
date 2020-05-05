module type API = sig
  type t

  type 'a p

  val keywords : string list

  val main : t p
end
