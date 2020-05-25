open Lambe_ast.Term

let pp_native ppf = function
  | Integer f -> Format.fprintf ppf "%i" f
  | Float f -> Format.fprintf ppf "%f" f
  | String s -> Format.fprintf ppf "\"%s\"" s
  | Char c -> Format.fprintf ppf "'%c'" c

let rec pp_case ppf = function
  | [] -> ()
  | (t, e) :: l -> Format.fprintf ppf "@ %a-> %a%a" Type.pp t pp e pp_case l

and pp_when_let ppf = function
  | None -> ()
  | Some n -> Format.fprintf ppf "let %s = " n

and pp_when ppf = function
  | w, e -> Format.fprintf ppf " when %a%a" pp_when_let w pp e

and pp_with ppf = function
  | [] -> ()
  | (n, t) :: l -> Format.fprintf ppf " with %s=%a%a" n pp t pp_with l

and pp ppf = function
  | Literal n -> Format.fprintf ppf "%a" pp_native n
  | Variable s -> Format.fprintf ppf "%s" s
  | Abstraction (n1, Abstraction (n2, t)) ->
    Format.fprintf ppf "{@[<v>%s %s ->@ %a@]@ }" n1 n2 pp t
  | Abstraction (n, t) -> Format.fprintf ppf "{ @[<v>%s ->@ %a@]@ }" n pp t
  | Apply (t1, (Apply (_, _) as t2)) -> Format.fprintf ppf "%a (%a)" pp t1 pp t2
  | Apply (t1, t2) -> Format.fprintf ppf "%a %a" pp t1 pp t2
  | Let (n, t1, t2) ->
    Format.fprintf ppf "@[<v>let@[<v> %s =@ %a@]@ in %a@]" n pp t1 pp t2
  | When (n, c) -> Format.fprintf ppf "@[<v>%a%a@]" pp_when n pp_case c
  | With (t, l) -> Format.fprintf ppf "%a%a" pp t pp_with l
