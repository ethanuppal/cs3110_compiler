open X86ISTMB
open Alcotest
open Platform

let e2e_root = Util.merge_paths [ Project_root.path; "test/e2e" ]

let command_prefix platform =
  match (platform.os, platform.cpu_arch) with
  | MacOS _, Arm -> "arch -x86_64"
  | _ -> ""

let make_e2e_test filename source () =
  let expected = Test_snapshots.ir_transform filename source in
  Driver.compile [ filename ] [] (Some Test_bin.path);
  let actual =
    Util.get_command_output
      ((Platform.get_platform () |> command_prefix)
      ^ " "
      ^ Util.merge_paths [ Test_bin.path; "build_dir/a.out" ])
  in
  (check string) "Compiled output should match IR simulator" expected actual

let test_suite =
  ( "lib/backend/asm_emit.ml",
    Sys.readdir e2e_root |> Array.to_list
    |> List.map (fun filename ->
           let path = Util.merge_paths [ e2e_root; filename ] in
           test_case filename `Quick (make_e2e_test path (Util.read_file path)))
  )
