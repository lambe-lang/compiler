let () =
  let open Alcotest in
  run "checker" [ Kind_checker.test_cases ]
