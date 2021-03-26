module Render = struct
  let rec pp ppf =
    let open Lambe_ast.Type in
    function
    | Variable (n, _) -> Format.fprintf ppf "%s" n
    | Arrow (l, r, _) -> Format.fprintf ppf "(%a → %a)" pp l pp r
    | Invoke (l, r, _) ->
      Format.fprintf ppf "self →􏰆􏰆 %a for %a" pp r pp l
    | Union (l, r, _) -> Format.fprintf ppf "(%a | %a)" pp l pp r
    | Apply (l, r, _) -> Format.fprintf ppf "%a (%a)" pp l pp r
    | Lambda (n, k, t, _) ->
      Format.fprintf ppf "Λ(%s:%a).(%a)" n Kind.Render.pp k pp t
    | Forall (n, k, t, _) ->
      Format.fprintf ppf "∀(%s:%a). %a" n Kind.Render.pp k pp t
    | Exists (n, k, t, _) ->
      Format.fprintf ppf "∃(%s:%a). %a" n Kind.Render.pp k pp t
    | Rec (n, k, t, _) ->
      Format.fprintf ppf "μ(%s:%a).(%a)" n Kind.Render.pp k pp t
    | Const (n, l, _) -> Format.fprintf ppf "data %s %a" n pp_params l
    | Trait (g, _) -> pp_gamma ppf g
    | Use (t, t', _) -> Format.fprintf ppf "(%a).(%a)" pp t pp t'

  and pp_gamma ppf = function
    | Gamma (k, t, s, w) ->
      Format.fprintf ppf "trait %a{%a%a%a}" pp_with w pp_kinds k pp_types t
        pp_sigs s

  and pp_with ppf = function
    | [] -> ()
    | g :: l -> Format.fprintf ppf "with %a%a" pp_gamma g pp_with l

  and pp_kinds ppf = function
    | [] -> ()
    | (n, k) :: l ->
      Format.fprintf ppf " kind %s=%a%a" n Kind.Render.pp k pp_kinds l

  and pp_types ppf = function
    | [] -> ()
    | (n, t) :: l -> Format.fprintf ppf " type %s=%a%a" n pp t pp_types l

  and pp_sigs ppf = function
    | [] -> ()
    | (n, s) :: l -> Format.fprintf ppf " sig %s:%a%a" n pp s pp_sigs l

  and pp_params ppf = function
    | [] -> ()
    | [ (n, t) ] -> Format.fprintf ppf "(%s:%a)" n pp t
    | (n, t) :: l -> Format.fprintf ppf "(%s:%a) %a" n pp t pp_params l

  let check ppf t k = Format.fprintf ppf "%a :? %a @." pp t Kind.Render.pp k

  let subtype ppf t1 t2 = Format.fprintf ppf "%a <? %a @." pp t1 pp t2

  let reduce ppf t ot =
    let pp_option ppf = function
      | None -> Format.fprintf ppf "error"
      | Some t -> pp ppf t
    in
    Format.fprintf ppf "%a --> %a @." pp t pp_option ot
end
