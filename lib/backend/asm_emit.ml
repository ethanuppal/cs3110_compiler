let mangle name =
  let rec mangle_helper = function
    | [ last ] -> "_S" ^ last
    | namespace :: rest -> "_N" ^ namespace ^ mangle_helper rest
    | _ -> failwith "empty name"
  in
  "_x86istmb" ^ mangle_helper name

let debug_print_symbol = mangle [ "std"; "debug_print" ]
let stack_alignment = 16
let var_size = 8

let align_offset bytes =
  let amount_over = bytes mod stack_alignment in
  if amount_over = 0 then 0 else stack_alignment - amount_over

let emit_var regalloc var =
  match VariableMap.find regalloc var with
  | Regalloc.Register reg -> Asm.Operand.Register reg
  | Spill i -> Asm.Operand.Deref (RBP, (-var_size * i) - var_size)

let emit_oper regalloc = function
  | Operand.Variable var -> emit_var regalloc var
  | Constant int -> Asm.Operand.Intermediate int

(** [emit_save_registers text registers] emits instructions into [text] to save
    [registers] on the stack. If the register contents are to be restored, they
    must be restored using [emit_restore_registers]. *)
let emit_save_registers text registers =
  let open Asm.Instruction in
  let push_instructions = List.map (fun reg -> Push (Register reg)) registers in
  let added_size = var_size * List.length registers in
  let extra_offset = align_offset added_size in
  Asm.Section.add_all text push_instructions;
  Asm.Section.add text (Sub (Register RSP, Intermediate extra_offset))

(** [emit_save_registers text registers] emits instructions into [text] to pop
    [registers] from the stack. The registers being restored must be the same as
    those most recently pushed using [emit_save_registers]. *)
let emit_restore_registers text registers =
  let open Asm.Instruction in
  let pop_instructions =
    List.rev_map (fun reg -> Pop (Register reg)) registers
  in
  let added_size = var_size * List.length registers in
  let extra_offset = align_offset added_size in
  Asm.Section.add text (Add (Register RSP, Intermediate extra_offset));
  Asm.Section.add_all text pop_instructions

let emit_call text regalloc name args =
  emit_save_registers text Register.caller_saved_data_registers;
  let param_moves =
    Util.zip_shortest args Register.parameter_registers
    |> List.map (fun (arg, reg) ->
           Asm.Instruction.Mov (Register reg, emit_oper regalloc arg))
  in
  Asm.Section.add_all text param_moves;
  Asm.Section.add text (Asm.Instruction.Call (Label name));
  emit_restore_registers text Register.caller_saved_data_registers

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
      emit_restore_registers text Register.callee_saved_data_registers;
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
    VariableMap.fold
      (fun _var alloc acc ->
        match alloc with
        | Regalloc.Spill count -> max count acc
        | _ -> acc)
      regalloc 0
  in
  (* max_spill starts at zero but we need to start from rbp-8 *)
  let spill_bytes = var_size * (max_spill + 1) in
  let stack_bytes = spill_bytes + align_offset spill_bytes in

  let entry = Cfg.entry_to cfg in
  Asm.Section.add text
    (Label
       (Asm.Label.make ~is_global:true ~is_external:false
          (mangle (Cfg.name_of cfg))));
  (* no need to align here, we can assume as a callee that 8 bytes for the
     return address was already pushed to the stack. *)
  Asm.Section.add_all text
    [
      Push (Register RBP);
      Mov (Register RBP, Register RSP);
      Sub (Register RSP, Intermediate stack_bytes);
    ];
  (* restore is done at returns *)
  emit_save_registers text Register.callee_saved_data_registers;
  Asm.Section.add text (Jmp (Label (Basic_block.label_for entry)));
  Cfg.blocks_of cfg |> List.iter (emit_bb text cfg regalloc)
