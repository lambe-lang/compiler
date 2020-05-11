let should_parse input =
  let module CharParser = Transept.Extension.Parser.For_char_list in
  let open Lambe.Syntax.Parser.Make (CharParser) in
  let expected = Ok ()
  and computed =
    Response.fold
      (parse (optrep Entity.main <& eos) @@ stream input)
      (fun _ -> Ok ())
      (fun (s, _) -> Error (Stream.position s))
  in
  Alcotest.(check (result unit int)) "should_parse" expected computed

let cases = [ "algebraic"; "bool"; "control"; "option"; "product"; "zipper" ]

let test_cases =
  let open Alcotest in
  ( "Source Parser"
  , List.map
      (fun input ->
        test_case ("Should parse " ^ input) `Quick (fun () ->
            let filename = "sources/" ^ input ^ ".lambe" in
            let t = Sys.time () in
            let fx = should_parse @@ Ioutils.read_fully filename in
            let () = Printf.printf "Execution time: %fs\n" (Sys.time () -. t) in
            fx))
      cases )
