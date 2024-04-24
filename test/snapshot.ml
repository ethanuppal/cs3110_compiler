open Alcotest
open X86ISTMB

type transform = string -> string

let make_test_suite root suite transform =
  let open Util in
  let snapshots_folder = Util.merge_paths [ Project_root.path; root; suite ] in
  let files = Sys.readdir snapshots_folder in
  let snapshots =
    files |> Array.to_list
    |> List.map Filename.remove_extension
    |> List.sort_uniq String.compare
  in
  let snapshot_test (snapshot : string) () =
    let input_path = Util.merge_paths [ snapshots_folder; snapshot ^ ".in" ] in
    let output_path =
      Util.merge_paths [ snapshots_folder; snapshot ^ ".out" ]
    in
    let input = read_file input_path in
    let expected = read_file output_path in
    try
      let actual = transform input in
      (check string)
        "Using the given input transformer should yield matching output to the \
         expected."
        expected actual
    with Parse_lex.ParseError msg -> fail msg
  in
  let suite_name = Util.merge_paths [ root; suite ] in
  let snapshot_tests =
    snapshots
    |> List.map (fun snapshot -> (snapshot, `Quick, snapshot_test snapshot))
  in
  (suite_name, snapshot_tests)
