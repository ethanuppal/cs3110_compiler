open Alcotest
open Cs3110_compiler

type transform = string -> string -> string

let make_test_suite ?(version = Meta.get.version) root transform =
  let open Util in
  let version_string = Meta.Version.to_string version in
  let snapshot_folder =
    Util.merge_paths [ Project_root.path; root; version_string ]
  in
  print_endline snapshot_folder;
  let files = Sys.readdir snapshot_folder in
  let snapshots =
    files |> Array.to_list
    |> List.filter (fun file -> not (String.get file 0 = '.'))
    |> List.map (String.split_on_char '.' >> List.hd)
    |> List.sort_uniq String.compare
  in
  let snapshot_test (snapshot : string) () =
    let input_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".in" ] in
    let output_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".out" ] in
    let input = read_file input_path in
    let expected = read_file output_path in
    try
      let actual = transform (snapshot ^ ".in") input in
      (check string)
        "Using the given input transformer should yield matching output to the \
         expected."
        expected actual
    with Parse_lex.ParseError msg -> fail msg
  in
  let suite_name =
    Printf.sprintf "Snapshot Test Suite (testing version %s)" version_string
  in
  let snapshot_tests =
    snapshots
    |> List.map (fun snapshot -> (snapshot, `Slow, snapshot_test snapshot))
  in
  (suite_name, snapshot_tests)
