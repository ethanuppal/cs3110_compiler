let snapshots_root = "test/snapshots"

let type_suite =
  let transform filename input =
    let open X86ISTMB in
    try
      let statements = Parse_lex.lex_and_parse ~filename input in
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
  let statements = Parse_lex.lex_and_parse ~filename input in
  Analysis.infer statements;
  let cfgs = Ir_gen.generate statements in
  List.iter (Liveliness.analysis_of >> ignore) cfgs;
  let simulator = Ir_sim.make () in
  Ir_sim.run simulator cfgs;
  Ir_sim.output_of simulator

let ir_suite =
  Snapshot.make_test_suite snapshots_root "ir" (ir_transform, `Quick)

(** not sure why this is separate from [ir_suite]. *)
let basic_suite =
  Snapshot.make_test_suite snapshots_root "basic" (ir_transform, `Quick)

let parse_transform filename input =
  let open X86ISTMB in
  try
    Parse_lex.lex_and_parse ~filename input |> ignore;
    ""
  with Parse_lex.ParserError err -> err ^ "\n"

let parse_suite =
  Snapshot.make_test_suite snapshots_root "parse" (parse_transform, `Quick)
