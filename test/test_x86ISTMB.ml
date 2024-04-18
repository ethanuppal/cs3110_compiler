let () =
  [
    Test_util.test_suite;
    Test_id.test_suite;
    Test_snapshots.test_suite;
    Test_digraph.test_suite;
  ]
  |> Alcotest.run "x86ISTMB"
