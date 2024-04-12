open X86ISTMB
open Alcotest

type transform = string -> string -> string

(** Used to categorize snapshots by validity in version. *)
let version_string = Meta.Version.to_string Meta.get.version

let make_test_suite (root : string) (transform : transform) : unit test =
  let open Util in
  let snapshot_folder =
    Util.merge_paths [ Project_root.path; root; version_string ]
  in
  print_endline snapshot_folder;
  let files = Sys.readdir snapshot_folder in
  let snapshots =
    files |> Array.to_list
    |> List.map (String.split_on_char '.' >> List.hd)
    |> List.sort_uniq String.compare
  in
  let snapshot_test (snapshot : string) () =
    let input_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".in" ] in
    let output_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".out" ] in
    let input = read_file input_path in
    let expected = read_file output_path in
    try
      let actual = transform input_path input in
      (check string) "output equality" expected actual
    with Frontend.ParseLex.ParseError msg -> fail msg
  in
  let suite_name =
    Printf.sprintf "Snapshot Test Suite (testing version %s)" version_string
  in
  let snapshot_tests =
    snapshots
    |> List.map (fun snapshot -> (snapshot, `Slow, snapshot_test snapshot))
  in
  (suite_name, snapshot_tests)
