module Ast = Lambe_ast
module Syntax = Lambe_syntax
module Checker = Lambe_checker
module Render = Lambe_render

let read_fully filename =
  let text = ref "" in
  let chan = open_in filename in
  try
    while true do
      text := !text ^ input_line chan ^ "\n"
    done;
    !text
  with
  | End_of_file ->
    close_in chan;
    !text

let parse content =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Syntax.Parser.Make (CharParser) in
  Response.fold
    (parse (Entity.main <& eos) @@ stream content)
    (fun (_, a, _) -> Ok a)
    (fun (s, _) -> Error (Stream.position s))

let () =
  let filenme = Sys.argv.(1) in
  let content = read_fully filenme in
  let response = parse content in
  match response with
  | Ok a -> Render.Entity.pp Format.std_formatter a
  | Error s -> print_string ("Error at char " ^ string_of_int s ^ "\n")
