open Lambe_ast.Entity

let rec pp_with ppf = function
  | [] -> ()
  | a :: l -> Format.fprintf ppf " with %a %a" Type.pp a pp_with l

let pp_for ppf = function
  | None -> ()
  | Some e -> Format.fprintf ppf " for %a" Type.pp e

let rec pp_params ppf = function
  | [] -> ()
  | (a, t) :: l -> Format.fprintf ppf "(%s:%a) %a" a Kind.pp t pp_params l

let rec pp_data_attributes ppf = function
  | [] -> ()
  | [ (a, t) ] -> Format.fprintf ppf "%s:%a" a Type.pp t
  | (a, t) :: l ->
    Format.fprintf ppf "%s:%a; %a" a Type.pp t pp_data_attributes l

let rec pp_enum ppf = function
  | [] -> ()
  | [ t ] -> Format.fprintf ppf "%a" Type.pp t
  | t :: l -> Format.fprintf ppf "%a | %a" Type.pp t pp_enum l

let pp ppf = function
  | Kind (n, t) -> Format.fprintf ppf "kind %s = %a" n Kind.pp t
  | Sig (n, t, f, w) ->
    Format.fprintf ppf "sig %s : %a %a { %a }" n Type.pp t pp_for f pp_with w
  | Def (n, t) -> Format.fprintf ppf "def %s = %a" n Term.pp t
  | Data (n, p, l) ->
    Format.fprintf ppf "data %s %a %a" n pp_params p pp_data_attributes l
  | Type (n, p, t) -> Format.fprintf ppf "type %s %a %a" n pp_params p Type.pp t
  | Enum (n, p, l) -> Format.fprintf ppf "type %s %a %a" n pp_params p pp_enum l
  | _ -> ()
