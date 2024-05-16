module ConstFold : Pass.Sig = struct
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

module CopyProp : Pass.Sig = struct
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
      | Assign (var, oper) ->
          VariableMap.replace vals var oper;
          Basic_block.set_ir bb i (Assign (var, subs oper))
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

let apply passes cfg liveliness =
  let apply_pass pass bb =
    Pass.execute pass bb (IdMap.find liveliness (Basic_block.id_of bb))
  in
  passes |> List.iter (fun pass -> Cfg.iter (apply_pass pass) cfg)
