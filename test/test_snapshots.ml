let snapshots_root = "test/snapshots"

let type_suite =
  let transform input =
    let open X86ISTMB in
    try
      let statements = Parse_lex.lex_and_parse input in
      Analysis.infer statements;
      statements |> List.map Ast.stmt_to_string |> String.concat ""
    with
    | Analysis.TypeInferenceError err ->
        Printexc.to_string (Analysis.TypeInferenceError err) ^ "\n"
    | e -> raise e
  in
  Snapshot.make_test_suite snapshots_root "type" transform

let ir_suite =
  let transform input =
    let open X86ISTMB in
    let statements = Parse_lex.lex_and_parse input in
    Analysis.infer statements;
    let ir = Ir_gen.generate statements in
    let main_cfg = List.hd ir in
    let simulator = Simulator.make () in
    Simulator.simulate simulator main_cfg;
    Simulator.output_of simulator
  in
  Snapshot.make_test_suite snapshots_root "ir" transform
