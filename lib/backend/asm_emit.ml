let debug_print_symbol = "_x86istmb_debug_print"

let emit_var regalloc var =
  match Ir.VariableMap.find regalloc var with
  | Regalloc.Register reg -> Asm.Operand.Register reg
  | Spill i -> Asm.Operand.Deref (RSP, i)

let emit_oper regalloc = function
  | Operand.Variable var -> emit_var regalloc var
  | Constant int -> Asm.Operand.Intermediate int

let emit_call text regalloc name args =
  let to_save = Asm.Register.caller_saved in
  let to_save =
    if List.length to_save mod 2 = 0 then to_save
    else List.hd to_save :: to_save
  in
  Asm.Section.add_all text
    (List.map (fun r -> Asm.Instruction.Push (Register r)) to_save
    @ [
        (* double push for 16 byte alignment *)
        Asm.Instruction.Mov (Register RDI, List.hd args |> emit_oper regalloc);
        Call (Label name);
      ]
    @ (List.map (fun r -> Asm.Instruction.Pop (Register r)) to_save |> List.rev)
    )

(** *)
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
  | Call _ -> failwith "TODO"
  | Return op ->
      Asm.Section.add text (Mov (Register RAX, emit_oper regalloc op))

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
  Asm.Section.add_all text
    [
      Label
        (Asm.Label.make ~is_global:true ~is_external:false
           ("_x86istmb_" ^ Cfg.name_of cfg));
      Push (Register RBP);
      Mov (Register RBP, Register RSP);
    ];
  Cfg.blocks_of cfg |> List.iter (emit_bb text cfg regalloc);
  Asm.Section.add_all text
    [ Mov (Register RSP, Register RBP); Pop (Register RBP); Ret ]
