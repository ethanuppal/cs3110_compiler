module ConstFold : Pass.PASS = struct
  let const_fold (bb, _) =
    for i = 0 to Basic_block.length_of bb - 1 do
      match Basic_block.get_ir bb i with
      | Add (var, Operand.Constant lhs, Operand.Constant rhs) ->
          Basic_block.set_ir bb i
            (Ir.Assign (var, Operand.make_const (lhs + rhs)))
      | Sub (var, Operand.Constant lhs, Operand.Constant rhs) ->
          Basic_block.set_ir bb i
            (Ir.Assign (var, Operand.make_const (lhs - rhs)))
      | _ -> ()
    done

  let pass = Pass.make const_fold
end

module CopyProp : Pass.PASS = struct
  module VariableMap = Hashtbl.Make (Variable)

  let copy_prop (bb, _) =
    let vals = VariableMap.create 16 in
    let subs = function
      | Operand.Variable var -> (
          match VariableMap.find_opt vals var with
          | Some oper -> oper
          | None -> Operand.make_var var)
      | oper -> oper
    in
    for i = 0 to Basic_block.length_of bb - 1 do
      match Basic_block.get_ir bb i with
      | Assign (var, oper) -> VariableMap.replace vals var oper
      | Add (var, oper1, oper2) ->
          Basic_block.set_ir bb i (Add (var, subs oper1, subs oper2))
      | Sub (var, oper1, oper2) ->
          Basic_block.set_ir bb i (Sub (var, subs oper1, subs oper2))
      | TestEqual (var, oper1, oper2) ->
          Basic_block.set_ir bb i (TestEqual (var, subs oper1, subs oper2))
      | Ref (var, oper) -> Basic_block.set_ir bb i (Ref (var, subs oper))
      | Deref (var, oper) -> Basic_block.set_ir bb i (Deref (var, subs oper))
      | _ -> ()
    done

  let pass = Pass.make copy_prop
end

module DeadCode : Pass.PASS = struct
  let dead_code (bb, analysis) =
    let length = Basic_block.length_of bb in
    for rev_i = 0 to Basic_block.length_of bb - 1 do
      let i = length - rev_i - 1 in
      let live_out =
        Liveliness.BasicBlockAnalysis.live_after_instr analysis
          (Basic_block.get_orig_idx bb i)
      in
      match Basic_block.get_ir bb i |> Ir.kill_of with
      | Some var ->
          if not (Liveliness.VariableSet.mem var live_out) then
            Basic_block.rem_ir bb i
      | None -> ()
    done

  let pass = Pass.make dead_code
end

let apply passes cfg liveliness =
  let apply_pass pass bb =
    Pass.execute pass bb (Util.IdMap.find liveliness (Basic_block.id_of bb))
  in
  passes |> List.iter (fun pass -> Cfg.iter (apply_pass pass) cfg)
