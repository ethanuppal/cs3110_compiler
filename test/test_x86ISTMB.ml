let () =
  [
    Test_util.test_suite;
    Test_id.test_suite;
    Test_snapshots.ir_suite;
    Test_snapshots.type_suite;
    Test_digraph.test_suite;
    Test_context.suite;
  ]
  |> Alcotest.run "x86ISTMB"
