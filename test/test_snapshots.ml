let snapshots_root = "test/snapshots"

let type_suite =
  let transform filename input =
    let open X86ISTMB in
    try
      let statements = ParseLex.lex_and_parse ~filename input in
      Analysis.infer statements;
      statements |> List.map Ast.stmt_to_string |> String.concat ""
    with
    | Analysis.AnalyzerError err ->
        Printexc.to_string (Analysis.AnalyzerError err) ^ "\n"
    | e -> raise e
  in
  Snapshot.make_test_suite snapshots_root "type" (transform, `Quick)

(** [ir_transform filename input] is the result of running the IR simulator on
    the source code [input]. *)
let ir_transform filename input =
  let open X86ISTMB in
  let open Util in
  let statements = ParseLex.lex_and_parse ~filename input in
  Analysis.infer statements;
  let cfgs, _, _ = IrGen.generate statements in
  List.iter (Liveliness.analysis_of >> ignore) cfgs;
  let simulator = IrSim.make () in
  IrSim.run simulator cfgs;
  IrSim.output_of simulator

let ir_suite =
  Snapshot.make_test_suite snapshots_root "ir" (ir_transform, `Quick)

let parse_transform filename input =
  let open X86ISTMB in
  try
    ParseLex.lex_and_parse ~filename input |> ignore;
    ""
  with ParseLex.ParserError err -> err ^ "\n"

let parse_suite =
  Snapshot.make_test_suite snapshots_root "parse" (parse_transform, `Quick)

let compile_transform filename _ =
  let open X86ISTMB in
  Driver.compile [ filename ] [] (Some Test_bin.path);
  Util.get_command_output
    (Platform.(get_platform () |> command_prefix)
    ^ " "
    ^ Util.merge_paths [ Test_bin.path; "build_dir/a.out" ])

let compile_suite =
  Snapshot.make_test_suite snapshots_root "compile" (ir_transform, `Quick)
