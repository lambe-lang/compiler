let () =
  Alcotest.(run "checker" [ Kind_test.test_cases; Type_test.test_cases; Term_test.test_cases ])
