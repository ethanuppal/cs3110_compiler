open X86ISTMB
open Alcotest
open Platform

let e2e_root = Util.merge_paths [ Project_root.path; "test/e2e" ]
let test_bin = Util.merge_paths [ Project_root.path; "test/bin" ]

let e2e_test filename source flags () =
  let expected = Test_snapshots.ir_transform filename source in
  Driver.compile [ filename ] flags (Some test_bin);
  let actual =
    Util.get_command_output
      ((Platform.get_platform () |> command_prefix)
      ^ " "
      ^ Util.merge_paths [ test_bin; "build_dir/a.out" ])
  in
  (check string) "Compiled output should match IR simulator" expected actual

let files = Sys.readdir e2e_root |> Array.to_list

let unoptimized =
  List.map
    (fun filename ->
      let path = Util.merge_paths [ e2e_root; filename ] in
      let test_name = Printf.sprintf "%s (unoptimized)" filename in
      test_case test_name `Quick (e2e_test path (Util.read_file path) []))
    files

let optimized =
  List.map
    (fun filename ->
      let path = Util.merge_paths [ e2e_root; filename ] in
      let test_name = Printf.sprintf "%s (optimized)" filename in
      test_case test_name `Quick
        (e2e_test path (Util.read_file path) [ Driver.Optimize ]))
    files

let test_suite = ("lib/user/driver.ml", unoptimized @ optimized)
