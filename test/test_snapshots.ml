let test_suite =
  let transform path input =
    let open X86ISTMB in
    if String.starts_with ~prefix:"type" path then
      try
        let statements = Parse_lex.lex_and_parse input in
        Analysis.infer statements;
        statements |> List.map Ast.stmt_to_string |> String.concat ""
      with
      | Analysis.TypeInferenceError err ->
          Printexc.to_string (Analysis.TypeInferenceError err) ^ "\n"
      | e -> raise e
    else if String.starts_with ~prefix:"ir" path then (
      let statements = Parse_lex.lex_and_parse input in
      Analysis.infer statements;
      let ir = Ir_gen.generate statements in
      let main_cfg = List.hd ir in
      let simulator = Simulator.make () in
      Simulator.simulate simulator main_cfg;
      Simulator.output_of simulator)
    else ""
  in
  Snapshot.make_test_suite "test/snapshots" transform
