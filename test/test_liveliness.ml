open Alcotest
open X86ISTMB

let set_of_list list =
  List.fold_right Liveliness.VariableSet.add list Liveliness.VariableSet.empty

(* TODO: massive todo for the file is handle cycles in cfg and other more
   complicated programs *)

(** The program tested is: [
      one_instruction_test { i0 = i1 + i2 }
    ] *)
let one_instruction_test () =
  let cfg = Cfg.make "one_instruction_test" in
  let bb = Cfg.entry_to cfg in
  let i0 = Variable.make () in
  let i1 = Variable.make () in
  let i2 = Variable.make () in
  Basic_block.add_ir bb (Ir.Add (i0, Operand.make_var i1, Operand.make_var i2));
  let _, analysis =
    Liveliness.analysis_of cfg |> Util.IdMap.to_seq |> List.of_seq |> List.hd
  in
  (check bool) "live_in should contain read-only variable" true
    (Liveliness.VariableSet.mem i1
       (Liveliness.BasicBlockAnalysis.live_in analysis));
  (check bool) "live_in should contain read-only variable" true
    (Liveliness.VariableSet.mem i2
       (Liveliness.BasicBlockAnalysis.live_in analysis));
  (check bool) "live_out for terminal basic block should be empty" true
    (Liveliness.VariableSet.is_empty
       (Liveliness.BasicBlockAnalysis.live_out analysis))

(** The program tested is:
    [
      two_instruction_test { i0 = i1 + i2; i4 = i1 + i3 }
    ] *)
let two_instruction_test () =
  let cfg = Cfg.make "two_instruction_test" in
  let bb = Cfg.entry_to cfg in
  let i0 = Variable.make () in
  let i1 = Variable.make () in
  let i2 = Variable.make () in
  let i3 = Variable.make () in
  let i4 = Variable.make () in
  Basic_block.add_ir bb (Ir.Add (i0, Operand.make_var i1, Operand.make_var i2));
  Basic_block.add_ir bb (Ir.Add (i4, Operand.make_var i1, Operand.make_var i3));
  let _, analysis =
    Liveliness.analysis_of cfg |> Util.IdMap.to_seq |> List.of_seq |> List.hd
  in
  (* print_endline (Cfg.to_string cfg); print_endline
     (Liveliness.BasicBlockAnalysis.to_string analysis); *)
  (check bool)
    "variables are live if they are read from in this instruction and if they \
     are live in the next instruction and not modified in this instruction"
    true
    (Liveliness.VariableSet.equal
       (set_of_list [ i1; i2; i3 ])
       (Liveliness.BasicBlockAnalysis.live_before_instr analysis 0));
  (check bool)
    "live out of instruction is live in of next one when no branching is done"
    true
    (Liveliness.VariableSet.equal
       (set_of_list [ i1; i3 ])
       (Liveliness.BasicBlockAnalysis.live_after_instr analysis 0));
  (check bool) "variables are live if they are read from in this instruction"
    true
    (Liveliness.VariableSet.equal
       (set_of_list [ i1; i3 ])
       (Liveliness.BasicBlockAnalysis.live_before_instr analysis 1));
  (check bool) "live out of final instruction is empty" true
    (Liveliness.VariableSet.equal (set_of_list [])
       (Liveliness.BasicBlockAnalysis.live_after_instr analysis 1))

let two_basic_block_test () =
  let cfg = Cfg.make "two_instruction_test" in
  let bb = Cfg.entry_to cfg in
  let bb2 = Cfg.create_block cfg in
  Cfg.insert_unconditional cfg bb bb2;
  let i0 = Variable.make () in
  let i1 = Variable.make () in
  let i2 = Variable.make () in
  let i3 = Variable.make () in
  let i4 = Variable.make () in
  Basic_block.add_ir bb (Ir.Add (i0, Operand.make_var i1, Operand.make_var i2));
  Basic_block.add_ir bb2 (Ir.Add (i0, Operand.make_var i3, Operand.make_var i4));
  let analyses =
    Liveliness.analysis_of cfg |> Util.IdMap.to_seq |> List.of_seq
  in
  (check int)
    "the liveliness analysis should have returned analyses for both basic \
     blocks in the cfg"
    2 (List.length analyses);
  print_endline (Cfg.to_string cfg);
  List.iter
    (fun (id, analysis) ->
      Printf.printf ".L%d <=> " (Id.int_of id);
      print_endline (Liveliness.BasicBlockAnalysis.to_string analysis))
    analyses;
  List.iter
    (fun (id, analysis) ->
      if id = Basic_block.id_of bb then (
        (check bool)
          "variables are live if they are read from in this instruction and if \
           they are live in the next instruction and not modified in this \
           instruction"
          true
          (Liveliness.VariableSet.equal
             (set_of_list [ i1; i2; i3; i4 ])
             (Liveliness.BasicBlockAnalysis.live_before_instr analysis 0));
        (check bool)
          "live out of instruction is live in of next one when branching is \
           done"
          true
          (Liveliness.VariableSet.equal
             (set_of_list [ i3; i4 ])
             (Liveliness.BasicBlockAnalysis.live_after_instr analysis 0)))
      else (
        (check bool)
          "variables are live if they are read from in this instruction" true
          (Liveliness.VariableSet.equal
             (set_of_list [ i3; i4 ])
             (Liveliness.BasicBlockAnalysis.live_before_instr analysis 0));
        (check bool) "live out of final instruction is empty" true
          (Liveliness.VariableSet.equal (set_of_list [])
             (Liveliness.BasicBlockAnalysis.live_after_instr analysis 0))))
    analyses

let test_suite =
  ( "lib/backend/liveliness.ml",
    [
      test_case "one_instruction_test" `Quick one_instruction_test;
      test_case "two_instruction_test" `Quick two_instruction_test;
      test_case "two_basic_block_test" `Quick two_basic_block_test;
    ] )
