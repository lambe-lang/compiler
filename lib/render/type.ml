module Render = struct
  let rec pp ppf =
    let open Lambe_ast.Type in
    function
    | Variable (n, _) -> Format.fprintf ppf "%s" n
    | Arrow (l, r, _) -> Format.fprintf ppf "(%a -> %a)" pp l pp r
    | Invoke (l, r, _) -> Format.fprintf ppf "self -> %a for %a" pp r pp l
    | Union (l, r, _) -> Format.fprintf ppf "(%a | %a)" pp l pp r
    | Apply (l, r, _) -> Format.fprintf ppf "%a (%a)" pp l pp r
    | Lambda (n, k, t, _) ->
      Format.fprintf ppf "(%s:%a) -> (%a)" n Kind.Render.pp k pp t
    | Forall (n, k, t, _) ->
      Format.fprintf ppf "forall (%s:%a). %a" n Kind.Render.pp k pp t
    | Exists (n, k, t, _) ->
      Format.fprintf ppf "exists (%s:%a). %a" n Kind.Render.pp k pp t
    | Rec (n, t, _) -> Format.fprintf ppf "rec %s. %a" n pp t
    | Const (n, l, _) -> Format.fprintf ppf "data %s %a" n pp_params l
    | Trait _ -> Format.fprintf ppf "trait ..."
    | Use (t, t', _) -> Format.fprintf ppf "(%a).(%a)" pp t pp t'

  and pp_params ppf = function
    | [] -> ()
    | [ (n, t) ] -> Format.fprintf ppf "(%s:%a)" n pp t
    | (n, t) :: l -> Format.fprintf ppf "(%s:%a) %a" n pp t pp_params l

  let check ppf t k = Format.fprintf ppf "(%a) :? (%a) @." pp t Kind.Render.pp k

  let subtype ppf t1 t2 = Format.fprintf ppf "(%a) <? (%a) @." pp t1 pp t2

  let reduce ppf t ot =
    let pp_option ppf = function
      | None -> Format.fprintf ppf "error"
      | Some t -> pp ppf t
    in
    Format.fprintf ppf "(%a) --> (%a) @." pp t pp_option ot
end
