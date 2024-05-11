open X86ISTMB
open Alcotest

let e2e_root = Util.merge_paths [ Project_root.path; "test/e2e" ]
let test_bin = Util.merge_paths [ Project_root.path; "test/bin" ]

let make_e2e_test filename source () =
  let expected = Test_snapshots.ir_transform filename source in
  Driver.compile [ filename ] [] (Some test_bin);
  let actual =
    Util.get_command_output
      (Util.read_file
         (Util.merge_paths [ test_bin; "build_dir/cmd_prefix.txt" ])
      ^ " "
      ^ Util.merge_paths [ test_bin; "build_dir/a.out" ])
  in
  (check string) "Compiled output should match IR simulator" expected actual

let test_suite =
  ( "lib/backend/asm_emit.ml",
    Sys.readdir e2e_root |> Array.to_list
    |> List.map (fun filename ->
           let path = Util.merge_paths [ e2e_root; filename ] in
           test_case filename `Slow (make_e2e_test path (Util.read_file path)))
  )