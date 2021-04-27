module Render = struct
  let rec pp ppf =
    let open Lambe_ast.Expr in
    function
    | Variable (n, _) -> Format.fprintf ppf "%s" n
    | Lambda ("_", e, _) -> Format.fprintf ppf "{ %a }" pp e
    | Lambda (n, e, _) -> Format.fprintf ppf "{ %s -> %a }" n pp e
    | Method (e, _) -> Format.fprintf ppf "@{ %a }" pp e
    | Apply (e1, e2, _) -> Format.fprintf ppf "%a (%a)" pp e1 pp e2
    | Bind (n, e1, e2, _) ->
      Format.fprintf ppf "let %s = %a in %a" n pp e1 pp e2
    | Use (e1, e2, _) -> Format.fprintf ppf "(%a).(%a)" pp e1 pp e2
    | Trait (g, d, _) -> pp_gamma ppf (g, d)
    | When (n, l, _) -> Format.fprintf ppf "when %s %a" n pp_when l
    | Pack (t, e, (a, k, t'), s) ->
      Format.fprintf ppf "{ %a, %a } as %a" Type.Render.pp t pp e Type.Render.pp
        (Lambe_ast.Type.Exists (a, k, t', s))
    | Unpack (t, n, e1, e2, _) ->
      Format.fprintf ppf "let { %a, %s } = %a in %a" Type.Render.pp t n pp e1 pp
        e2
    | As (e, t, _) -> Format.fprintf ppf "%a as %a" pp e Type.Render.pp t

  and pp_when ppf = function
    | [] -> ()
    | (t, e) :: l ->
      Format.fprintf ppf "is %a -> %a %a" Type.Render.pp t pp e pp_when l

  and pp_gamma ppf = function
    | Gamma (k, t, s, w), d ->
      Format.fprintf ppf "trait %a{%a%a%a%a}" pp_with w pp_kinds k pp_types t
        pp_sigs s pp_defs d

  and pp_with ppf = function
    | [] -> ()
    | g :: l -> Format.fprintf ppf "with %a%a" pp_gamma (g, []) pp_with l

  and pp_kinds ppf = function
    | [] -> ()
    | (n, k) :: l ->
      Format.fprintf ppf " kind %s=%a%a" n Kind.Render.pp k pp_kinds l

  and pp_types ppf = function
    | [] -> ()
    | (n, t) :: l ->
      Format.fprintf ppf " type %s=%a%a" n Type.Render.pp t pp_types l

  and pp_sigs ppf = function
    | [] -> ()
    | (n, s) :: l ->
      Format.fprintf ppf " sig %s:%a%a" n Type.Render.pp s pp_sigs l

  and pp_defs ppf = function
    | [] -> ()
    | (n, s) :: l -> Format.fprintf ppf " def %s:%a%a" n pp s pp_defs l

  let check ppf e = function
    | Some t -> Format.fprintf ppf "%a : %a @." pp e Type.Render.pp t
    | None -> Format.fprintf ppf "%a : ? @." pp e
end
