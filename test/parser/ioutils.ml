let test_directory = "../../../../test/parser/"

(* TODO - change this ugly hardcoded path *)

let read_fully filename =
  let text = ref "" in
  let chan = open_in (test_directory ^ filename) in
  try
    while true do
      text := !text ^ input_line chan ^ "\n"
    done;
    !text
  with
  | End_of_file ->
    close_in chan;
    !text
