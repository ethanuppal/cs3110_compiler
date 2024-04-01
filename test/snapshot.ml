open OUnit2
open X86ISTMB
open X86ISTMB.Frontend

(** Used to categorize snapshots by validity in version. *)
let version_string = Meta.Version.to_string Meta.get.version

let make_test_suite root =
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
  let snapshot_test snapshot _ =
    let interpreter = Interpreter.create () in
    let input_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".in" ] in
    let output_path = Util.merge_paths [ snapshot_folder; snapshot ^ ".out" ] in
    let input = read_file input_path in
    let output = read_file output_path in
    let stdout = ref "" in
    try
      let statements = ParseLex.lex_and_parse input in
      interpreter.set_mode (Text stdout);
      List.iter interpreter.step statements;
      assert_equal output !stdout
    with ParseLex.ParseError msg -> assert_failure msg
  in
  let snapshot_test_funcs : test_fun list = List.map snapshot_test snapshots in
  Printf.sprintf "Snapshot Test Suite (testing version %s)" version_string
  >::: (List.combine snapshots snapshot_test_funcs
       |> List.map (Util.uncurry ( >:: )))
