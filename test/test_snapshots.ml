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

(** [ir_transform] is the result of running the IR simulator on the source code
    [input]. *)
let ir_transform input =
  let open X86ISTMB in
  let statements = Parse_lex.lex_and_parse input in
  Analysis.infer statements;
  let ir = Ir_gen.generate statements in
  let main_cfg = List.hd ir in
  let simulator = Ir_sim.make () in
  Ir_sim.run simulator main_cfg;
  Ir_sim.output_of simulator

let ir_suite = Snapshot.make_test_suite snapshots_root "ir" ir_transform

(** not sure why this is separate from [ir_suite]. *)
let basic_suite = Snapshot.make_test_suite snapshots_root "basic" ir_transform
