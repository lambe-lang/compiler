let () =
  let open Alcotest in
  run "checker" [ Freevariables_test.test_cases; TypeChecker_test.test_cases ]
