open X86ISTMB
open Alcotest

let make_opts_test passes =
  let ir0_source =
    Util.read_file
      (Util.merge_paths [ Project_root.path; "test/printing_progs/0.x86istmb" ])
  in
  let statements = Parse_lex.lex_and_parse ir0_source in
  Analysis.infer statements;
  let cfgs, _, _ = Ir_gen.generate statements in
  let main_cfg = List.hd cfgs in
  let liveliness_analysis = Liveliness.analysis_of main_cfg in
  let simulator = Ir_sim.make () in
  Ir_sim.run simulator [ main_cfg ];
  let unopt_output = Ir_sim.output_of simulator in
  Passes.apply passes main_cfg liveliness_analysis;
  Ir_sim.clear_output simulator;
  Ir_sim.run simulator [ main_cfg ];
  let opt_output = Ir_sim.output_of simulator in
  (check string) "optimization should not change program behavior" unopt_output
    opt_output

let fixed_ir_opts_tests =
  [
    ([ Passes.ConstFold.pass ], "const fold ir opt");
    ([ Passes.CopyProp.pass ], "copy prop ir opt");
    ([ Passes.DeadCode.pass ], "dead code ir opt");
    ([ Passes.IntMult.pass ], "int mult ir opt");
    ( [
        Pass.combine
          [
            Passes.ConstFold.pass;
            Passes.CopyProp.pass;
            Passes.DeadCode.pass;
            Passes.IntMult.pass;
          ];
      ],
      "combined ir opt" );
    ( [
        Pass.sequence Passes.ConstFold.pass Passes.CopyProp.pass
        |> Pass.repeat 10;
        Passes.DeadCode.pass;
      ],
      "complex ir opt" );
  ]
  |> List.map (fun (passes, name) ->
         test_case name `Quick (fun () -> make_opts_test passes))

(* let qcheck_ir_opts () = let open QCheck in let test = let open QCheck.Gen in
   let pass_options = [| Passes.ConstFold.pass; Passes.CopyProp.pass;
   Passes.DeadCode.pass |] in let get_pass () = let* idx = int_bound
   (Array.length pass_options) in pass_options.(idx) in let gen_pass () = let
   rec gen_pass_aux temp pass = let* prob = float 1.0 in if prob < exp (-.temp)
   then pass else let* mutation = int_bound 3 in (match mutation with | 0 ->
   let* n = int_bound 10 in Pass.repeat n pass | 1 -> Pass.compose pass
   (get_pass ()) | 2 -> Pass.combine pass :: Seq.unfold () | _ -> pass) |>
   gen_pass_aux (temp +. 1.0) in gen_pass_aux 1.0 (get_pass ()) in let pass =
   gen_pass () in make_opts_test [ pass ] in QCheck_alcotest.to_alcotest
   ~long:true (Test.make ~name:"random ir opt passes" ~count:100 test) *)

let test_suite = ("lib/ir/passes.ml", fixed_ir_opts_tests)
