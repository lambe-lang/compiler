let () =
  let open Alcotest in
  run "checker"
    [ Kind_subsume.test_cases; Type_subsume.test_cases ]
