open X86ISTMB
open Alcotest

let make_opts_test passes =
  let ir0_source =
    Util.read_file
      (Util.merge_paths [ Project_root.path; "test/printing_progs/0.x86istmb" ])
  in
  let statements = ParseLex.lex_and_parse ir0_source in
  Analysis.infer statements;
  let cfgs = IrGen.generate statements in
  let main_cfg = List.hd cfgs in
  let liveliness_analysis = Liveliness.analysis_of main_cfg in
  let simulator = IrSim.make () in
  IrSim.run simulator [ main_cfg ];
  let unopt_output = IrSim.output_of simulator in
  Passes.apply passes main_cfg liveliness_analysis;
  IrSim.clear_output simulator;
  IrSim.run simulator [ main_cfg ];
  let opt_output = IrSim.output_of simulator in
  (check string) "optimization should not change program behavior" unopt_output
    opt_output

let fixed_ir_opts_tests =
  [
    ([ Passes.ConstFold.pass ], "const fold ir opt");
    ([ Passes.CopyProp.pass ], "copy prop ir opt");
    ( [ Pass.combine [ Passes.ConstFold.pass; Passes.CopyProp.pass ] ],
      "combined ir opt" );
    ( [
        Pass.sequence Passes.ConstFold.pass Passes.CopyProp.pass
        |> Pass.repeat 10;
      ],
      "complex ir opt" );
  ]
  |> List.map (fun (passes, name) ->
         test_case name `Quick (fun () -> make_opts_test passes))

let test_suite = ("lib/ir/passes.ml", fixed_ir_opts_tests)
