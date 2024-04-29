let test_suite =
  let open Alcotest in
  let open X86ISTMB in
  let generic_test () =
    let cfg = Cfg.make "LMAO" in
    let bb = Cfg.entry_to cfg in
    let i0 = Variable.make () in
    let i1 = Variable.make () in
    let i2 = Variable.make () in
    Basic_block.add_ir bb
      (Ir.Add (i0, Operand.make_var i1, Operand.make_var i2));
    let _, analysis =
      Liveliness.analysis_of cfg |> Util.IdMap.to_seq |> List.of_seq |> List.hd
    in
    (check bool) "live_in should contain read-only variable" true
      (Liveliness.VariableSet.mem i1 (Liveliness.Analysis.live_in analysis));
    (check bool) "live_in should contain read-only variable" true
      (Liveliness.VariableSet.mem i2 (Liveliness.Analysis.live_in analysis));
    (check bool) "live_out for terminal basic block should be empty" true
      (Liveliness.VariableSet.is_empty (Liveliness.Analysis.live_out analysis))
  in
  ("lib/backend/liveliness.ml", [ test_case "generic_test" `Quick generic_test ])
