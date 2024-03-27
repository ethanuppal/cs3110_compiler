open OUnit2

let () = Snapshot.make_test_suite "test/snapshots" |> run_test_tt_main
