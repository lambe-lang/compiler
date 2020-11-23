let () =
  let open Alcotest in
  run "checker" [ Variables_test.test_cases; Type_check_test.test_cases ]
