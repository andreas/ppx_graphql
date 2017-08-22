let () =
  Alcotest.run "ppx_graphql" [
    "parsing", Parsing_test.suite;
    "variables", Variables_test.suite;
    "integration", Integration_test.suite;
  ]
