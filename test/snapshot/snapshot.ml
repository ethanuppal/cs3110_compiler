open Alcotest
open X86ISTMB

type transform = (string -> string) * speed_level

let ignore_file_name = "IGNORE"

let make_test_suite root suite (transform_f, speed) =
  let open Util in
  let snapshots_folder = Util.merge_paths [ Project_root.path; root; suite ] in
  let files = Sys.readdir snapshots_folder in
  let snapshots =
    files |> Array.to_list
    |> List.map Filename.remove_extension
    |> List.sort_uniq String.compare
  in
  let ignore_path = Util.merge_paths [ snapshots_folder; ignore_file_name ] in
  let ignored_snapshots =
    if Sys.file_exists ignore_path then
      Util.read_file ignore_path
      |> Str.split (Str.regexp_string "\n")
      |> List.filter (String.length >> ( < ) 0)
    else []
  in
  let should_ignore_snapshot snapshot =
    List.mem snapshot ignored_snapshots || snapshot = ignore_file_name
  in
  let snapshot_test snapshot () =
    let input_path = Util.merge_paths [ snapshots_folder; snapshot ^ ".in" ] in
    let output_path =
      Util.merge_paths [ snapshots_folder; snapshot ^ ".out" ]
    in
    let input = read_file input_path in
    let expected = read_file output_path in
    try
      let actual = transform_f input in
      (check string)
        "Using the given input transformer should yield matching output to the \
         expected."
        expected actual
    with Parse_lex.ParseError msg -> fail msg
  in
  let suite_name = Util.merge_paths [ root; suite ] in
  let snapshot_tests =
    snapshots
    |> List.filter_map (fun snapshot ->
           if should_ignore_snapshot snapshot then None
           else Some (snapshot, speed, snapshot_test snapshot))
  in
  (suite_name, snapshot_tests)
