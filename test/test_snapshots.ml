let test_suite =
  let interpreter_transform path input =
    ignore path;
    let open X86ISTMB in
    let interpreter = Interpreter.create () in
    let stdout = ref "" in
    let statements = Parse_lex.lex_and_parse input in
    interpreter.set_mode (Text stdout);
    List.iter interpreter.step statements;
    stdout
  in
  let () = ignore interpreter_transform in
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
    else ""
  in
  Snapshot.make_test_suite "test/snapshots" transform
