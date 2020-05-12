open Lambe_ast.Term

let pp_native ppf = function
  | Integer f -> Format.fprintf ppf "%i" f
  | Float f -> Format.fprintf ppf "%f" f
  | String s -> Format.fprintf ppf "\"%s\"" s
  | Char c -> Format.fprintf ppf "'%c'" c

let rec pp_case ppf = function
  | [] -> ()
  | (t, e) :: l -> Format.fprintf ppf " is %a -> %a %a" Type.pp t pp e pp_case l

and pp_when_let ppf = function
  | None -> ()
  | Some n -> Format.fprintf ppf "let %s =" n

and pp_with ppf = function
  | [] -> ()
  | (n, t) :: l -> Format.fprintf ppf " with %s=%a%a" n pp t pp_with l

and pp ppf = function
  | Native n -> Format.fprintf ppf "%a" pp_native n
  | Variable s -> Format.fprintf ppf "%s" s
  | Abstraction (n, t) -> Format.fprintf ppf "{%s -> %a}" n pp t
  | Apply (t1, t2) -> Format.fprintf ppf "(%a) %a" pp t1 pp t2
  | Let (n, t1, t2) -> Format.fprintf ppf "let %s = %a in %a" n pp t1 pp t2
  | When (n, e, c) ->
    Format.fprintf ppf "when %a%a { %a }" pp_when_let n pp e pp_case c
  | With (t, l) -> Format.fprintf ppf "%a%a" pp t pp_with l
