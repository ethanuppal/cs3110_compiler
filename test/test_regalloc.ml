open X86ISTMB
open X86ISTMB.Util
open Alcotest

let allocations_same alloc1 alloc2 =
  match (alloc1, alloc2) with
  | Regalloc.Spill, Regalloc.Spill -> false
  | reg1, reg2 -> reg1 = reg2

let add_ir_list bb lst = List.iter (Basic_block.add_ir bb) lst

let basic_vars =
  let test () =
    let cfg = Cfg.make "basic_vars" in
    let entry = Cfg.entry_to cfg in

    let var0 = Variable.make () in
    let var1 = Variable.make () in
    add_ir_list entry
      [
        Ir.Assign (var0, Operand.make_const 1);
        Ir.Assign (var1, Operand.make_const 2);
        Ir.DebugPrint (Operand.make_var var0);
        Ir.DebugPrint (Operand.make_var var1);
      ];

    let liveliness = Liveliness.analysis_of cfg in
    let ordering = InstrOrdering.make cfg in
    let allocations = Regalloc.allocate_for cfg liveliness ordering in
    (check bool) "var0 and var1 are allocated separately" false
      (allocations_same
         (Ir.VariableMap.find allocations var0)
         (Ir.VariableMap.find allocations var1))
  in
  Alcotest.test_case "basic case" `Quick test

let write_after_dead =
  let test () =
    let cfg = Cfg.make "write_after_dead" in
    let entry = Cfg.entry_to cfg in

    let var0 = Variable.make () in
    let var1 = Variable.make () in
    add_ir_list entry
      [
        Ir.Assign (var0, Operand.make_const 1);
        Ir.Add (var1, Operand.make_var var0, Operand.make_const 2);
        Ir.Assign (var0, Operand.make_const 2);
        Ir.DebugPrint (Operand.make_var var1);
      ];

    let liveliness = Liveliness.analysis_of cfg in
    let ordering = InstrOrdering.make cfg in
    let allocations = Regalloc.allocate_for cfg liveliness ordering in
    (check bool) "var0 and var1 are allocated separately" false
      (allocations_same
         (Ir.VariableMap.find allocations var0)
         (Ir.VariableMap.find allocations var1))
  in
  Alcotest.test_case "write after dead" `Quick test

let spill_basic =
  let test () =
    let cfg = Cfg.make "spill_basic" in
    let entry = Cfg.entry_to cfg in
    let reg_count = List.length Regalloc.registers in
    let vars =
      Seq.of_dispenser (Variable.make >> Option.some)
      |> Seq.take (reg_count + 1)
      |> List.of_seq
    in

    add_ir_list entry
      (List.map (fun var -> Ir.Assign (var, Operand.make_const 0)) vars);
    add_ir_list entry
      (List.map (fun var -> Ir.DebugPrint (Operand.make_var var)) vars);

    let liveliness = Liveliness.analysis_of cfg in
    let ordering = InstrOrdering.make cfg in
    let allocations = Regalloc.allocate_for cfg liveliness ordering in
    let alloc_list = List.map (Ir.VariableMap.find allocations) vars in
    List.iteri
      (fun i var1 ->
        List.iteri
          (fun j var2 ->
            if i <> j then
              let name =
                Format.sprintf "var%i and var%i are allocated separately" i j
              in
              (check bool) name false (allocations_same var1 var2))
          alloc_list)
      alloc_list
  in
  Alcotest.test_case "spill_basic" `Quick test

let spill_special_case =
  let test () =
    let cfg = Cfg.make "spill_special_case" in
    let entry = Cfg.entry_to cfg in
    let reg_count = List.length Regalloc.registers in
    let vars =
      Seq.of_dispenser (Variable.make >> Option.some)
      |> Seq.take (reg_count + 1)
      |> List.of_seq
    in

    add_ir_list entry
      (List.map (fun var -> Ir.Assign (var, Operand.make_const 0)) vars);

    (* switch last two vars to trigger optimized spill behavior *)
    let bb_len = Basic_block.length_of entry in
    let last_ir = Basic_block.get_ir entry (bb_len - 1) in
    Basic_block.set_ir entry (bb_len - 1)
      (Basic_block.get_ir entry (bb_len - 2));
    Basic_block.set_ir entry (bb_len - 2) last_ir;

    add_ir_list entry
      (List.map (fun var -> Ir.DebugPrint (Operand.make_var var)) vars);

    let liveliness = Liveliness.analysis_of cfg in
    let ordering = InstrOrdering.make cfg in
    let allocations = Regalloc.allocate_for cfg liveliness ordering in
    let alloc_list = List.map (Ir.VariableMap.find allocations) vars in

    List.iteri
      (fun i var1 ->
        List.iteri
          (fun j var2 ->
            if i <> j then
              let name =
                Format.sprintf "var%i and var%i are allocated separately" i j
              in
              (check bool) name false (allocations_same var1 var2))
          alloc_list)
      alloc_list
  in
  Alcotest.test_case "spill_special_case" `Quick test

let test_suite =
  ( "lib/backend/regalloc/regalloc.ml",
    [ basic_vars; write_after_dead; spill_basic; spill_special_case ] )
