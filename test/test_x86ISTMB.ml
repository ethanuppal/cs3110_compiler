let () =
  [
    Test_util.test_suite;
    Test_id.test_suite;
    Test_snapshots.ir_suite;
    Test_snapshots.type_suite;
    Test_snapshots.basic_suite;
    Test_snapshots.parse_suite;
    Test_liveliness.test_suite;
    Test_e2e.test_suite;
    Test_passes.test_suite;
    Test_digraph.test_suite;
    Test_context.test_suite;
    Test_regalloc.test_suite;
  ]
  |> Alcotest.run "x86ISTMB"
