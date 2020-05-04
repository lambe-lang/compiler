let () =
  let open Alcotest in
  run "checker"
    [ Freevar_test.test_cases; Substitution_test.test_cases; Unification_test.test_cases ]
