let () =
  let open Alcotest in
  run "checker" [ Freevar_test.test_cases; Unify_test.test_cases ]
