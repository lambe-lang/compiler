open Lambe_ast.Entity

let rec pp_with ppf = function
  | [] -> ()
  | a :: l -> Format.fprintf ppf " with %a%a" Type.pp a pp_with l

let pp_for ppf = function
  | None -> ()
  | Some e -> Format.fprintf ppf " for %a" Type.pp e

let rec pp_params ppf = function
  | [] -> ()
  | (a, t) :: l -> Format.fprintf ppf " (%s:%a)%a" a Kind.pp t pp_params l

let rec pp_data_attributes ppf = function
  | [] -> ()
  | [ (a, t) ] -> Format.fprintf ppf "@ %s:%a" a Type.pp t
  | (a, t) :: l ->
    Format.fprintf ppf "@ %s:%a;%a" a Type.pp t pp_data_attributes l

let rec pp_enum ppf = function
  | [] -> ()
  | [ t ] -> Format.fprintf ppf "%a" Type.pp t
  | t :: l -> Format.fprintf ppf "%a | %a" Type.pp t pp_enum l

let pp_ident ppf s =
  let is_alpha s =
    let c = s.[0] in
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  in

  if is_alpha s then Format.fprintf ppf "%s" s else Format.fprintf ppf "(%s)" s

let rec pp ppf = function
  | Comment _ -> ()
  | Kind (n, t) ->
    Format.fprintf ppf "@[<v>kind %a = %a@ @]" pp_ident n Kind.pp t
  | Sig (n, t, f, w) ->
    Format.fprintf ppf "@[<v>sig %a : %a%a%a@]@ " pp_ident n Type.pp t pp_for f
      pp_with w
  | Def (n, t) ->
    Format.fprintf ppf "@[<v>def @[<v>%a =@ %a@]@ @]" pp_ident n Term.pp t
  | Data (n, p, l) ->
    Format.fprintf ppf "@[<v>data @[<v>%a%a {%a@]@ }@ @]" pp_ident n pp_params p
      pp_data_attributes l
  | Type (n, p, t) ->
    Format.fprintf ppf "@[<v>type %a%a = %a@ @]" pp_ident n pp_params p Type.pp
      t
  | Enum (n, p, l) ->
    Format.fprintf ppf "@[<v>type %a%a = %a@ @]" pp_ident n pp_params p pp_enum
      l
  | Trait (n, p, f, w, d) ->
    Format.fprintf ppf "@[<v>trait @[<v>%a%a%a%a {@ %a@]@ }@]" pp_ident n
      pp_params p pp_for f pp_with w pp_entities d
  | Impl ([], t, f, w, d) ->
    Format.fprintf ppf "@[<v>impl @[<v>%a%a%a {@ %a@]@ }@]" Type.pp t pp_for f
      pp_with w pp_entities d
  | Impl (p, t, f, w, d) ->
    Format.fprintf ppf "@[<v>impl @[<v>forall%a. %a%a%a {@ %a@]@ }@]" pp_params
      p Type.pp t pp_for f pp_with w pp_entities d

and pp_entities ppf = function
  | [] -> ()
  | [ e ] -> Format.fprintf ppf "%a" pp e
  | e :: l -> Format.fprintf ppf "%a@ %a" pp e pp_entities l
