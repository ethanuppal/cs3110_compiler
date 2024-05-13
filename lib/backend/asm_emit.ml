let mangle name = "_x86istmb_" ^ name
let debug_print_symbol = mangle "debug_print"
let var_size = 8

let emit_var regalloc var =
  match Ir.VariableMap.find regalloc var with
  | Regalloc.Register reg -> Asm.Operand.Register reg
  | Spill i -> Asm.Operand.Deref (RBP, (-var_size * i) - var_size)

let emit_oper regalloc = function
  | Operand.Variable var -> emit_var regalloc var
  | Constant int -> Asm.Operand.Intermediate int

let emit_call text regalloc name args =
  let to_save = Asm.Register.caller_saved in
  let to_save =
    if List.length to_save mod 2 = 0 then to_save
    else List.hd to_save :: to_save
  in
  let to_pass = [| Asm.Register.RDI; RSI; RDX; RCX; R8; R9 |] in
  Asm.Section.add_all text
    (List.map (fun r -> Asm.Instruction.Push (Register r)) to_save
    @ List.mapi
        (fun i arg ->
          Asm.Instruction.Mov (Register to_pass.(i), emit_oper regalloc arg))
        args
    @ [ Asm.Instruction.Call (Label name) ]
    @ (List.map (fun r -> Asm.Instruction.Pop (Register r)) to_save |> List.rev)
    )

let emit_ir text regalloc = function
  | Ir.Assign (var, op) ->
      Asm.Section.add text (Mov (emit_var regalloc var, emit_oper regalloc op))
  | Add (var, op, op2) ->
      Asm.Section.add_all text
        [
          Mov (emit_var regalloc var, emit_oper regalloc op);
          Add (emit_var regalloc var, emit_oper regalloc op2);
        ]
  | Sub (var, op, op2) | TestEqual (var, op, op2) ->
      Asm.Section.add_all text
        [
          Mov (emit_var regalloc var, emit_oper regalloc op);
          Sub (emit_var regalloc var, emit_oper regalloc op2);
        ]
  | Ref _ -> failwith "ref not impl"
  | Deref _ -> failwith "deref not impl"
  | DebugPrint op -> emit_call text regalloc debug_print_symbol [ op ]
  | Call (var, name, args) ->
      emit_call text regalloc (mangle name) args;
      Asm.Section.add text (Mov (emit_var regalloc var, Register RAX))
  | Return op_opt ->
      Option.map
        (fun op ->
          Asm.Section.add text (Mov (Register RAX, emit_oper regalloc op)))
        op_opt
      |> ignore;
      Asm.Section.add_all text
        [ Mov (Register RSP, Register RBP); Pop (Register RBP); Ret ]

let emit_bb text cfg regalloc bb =
  Asm.Section.add text
    (Label
       (Asm.Label.make ~is_global:false ~is_external:false
          (Basic_block.label_for bb)));
  bb |> Basic_block.to_list |> List.iter (emit_ir text regalloc);
  match Basic_block.condition_of bb with
  | Never | Conditional (Constant 0) -> ()
  | Always | Conditional (Constant _) ->
      let dest_bb = Cfg.take_branch cfg bb true |> Option.get in
      Asm.Section.add text (Jmp (Label (Basic_block.label_for dest_bb)))
  | Conditional op -> (
      let true_bb = Cfg.take_branch cfg bb true |> Option.get in
      let false_bb = Cfg.take_branch cfg bb false |> Option.get in
      match op with
      | Variable var ->
          Asm.Section.add text (Cmp (emit_var regalloc var, Intermediate 0));
          Asm.Section.add text (Je (Label (Basic_block.label_for false_bb)));
          Asm.Section.add text (Jmp (Label (Basic_block.label_for true_bb)))
      | Constant _ -> failwith "failure")

let emit_preamble ~text =
  Asm.Section.add text
    (Label
       (Asm.Label.make ~is_global:false ~is_external:true debug_print_symbol))

let emit_cfg ~text cfg regalloc =
  let max_spill =
    Ir.VariableMap.fold
      (fun _var alloc acc ->
        match alloc with
        | Regalloc.Spill count -> max count acc
        | _ -> acc)
      regalloc 0
  in
  (* max_spill starts at zero but we need to start from rbp-8 *)
  let spill_bytes = var_size * (max_spill + 1) in
  (* What we need to spill + however much is needed to round our total up to
     16 *)
  let total_bytes = spill_bytes + var_size in
  let stack_bytes = spill_bytes + (total_bytes mod 16) in
  Asm.Section.add_all text
    [
      Label
        (Asm.Label.make ~is_global:true ~is_external:false
           (mangle (Cfg.name_of cfg)));
      Push (Register RBP);
      Mov (Register RBP, Register RSP);
      Sub (Register RSP, Intermediate stack_bytes);
    ];
  Cfg.blocks_of cfg |> List.iter (emit_bb text cfg regalloc)
