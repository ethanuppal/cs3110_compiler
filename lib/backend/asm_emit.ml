module ParameterPassingContext = struct
  type t = {
    mutable pos : int;
    mutable regs : Asm.Register.t list;
  }

  let make () = { pos = 0; regs = Asm.Register.parameter_registers }

  let get_next ctx =
    if List.is_empty ctx.regs then (
      let pos = ctx.pos in
      ctx.pos <- ctx.pos + 1;
      ignore pos;
      failwith "i think should return spill with negative index")
    else
      let result = List.hd ctx.regs in
      ctx.regs <- List.tl ctx.regs;
      Regalloc.Register result
end

let platform = Platform.get_platform ()

let mangle_ffi name =
  match platform.os with
  | MacOS _ | Unknown -> "_" ^ name
  | Linux -> name

let mangle name =
  match name with
  | [ "ffi"; ffi_name ] -> mangle_ffi ffi_name
  | _ ->
      let rec mangle_helper = function
        | [ last ] -> "_S" ^ last
        | namespace :: rest -> "_N" ^ namespace ^ mangle_helper rest
        | _ -> failwith "empty name"
      in
      "_x86istmb" ^ mangle_helper name

let debug_print_int_symbol = mangle [ "std"; "debug_print_int" ]
let stack_alignment = 16
let var_size = 8

let align_offset bytes =
  let amount_over = bytes mod stack_alignment in
  if amount_over = 0 then 0 else stack_alignment - amount_over

let emit_var regalloc var =
  match VariableMap.find regalloc var with
  | Regalloc.Register reg -> Asm.Operand.Register reg
  | Spill i -> Asm.Operand.Deref (RBP, (-var_size * i) - var_size)

(* If emmitting a string literal, must provide a data section. *)
let emit_oper regalloc data_section = function
  | Operand.Variable var -> emit_var regalloc var
  | Constant int -> Asm.Operand.Intermediate int
  | StringLiteral str_lit ->
      let label_string = StringLiteral.label_for str_lit in
      Asm.Section.add_all data_section
        [
          Asm.Instruction.Label
            (Asm.Label.make ~is_global:false ~is_external:false label_string);
          Asm.Instruction.DataBytes
            ((StringLiteral.value_of str_lit
             |> String.to_seq |> List.of_seq |> List.map Char.code)
            @ [ 0 ]);
        ];
      Asm.Operand.RelativeLabel label_string

(** [make_mov] correctly produces an assembly instruction to assign [src_op] to
    [dest_loc] in the context of [regalloc] and [data_section]. *)
let make_mov regalloc data_section dest_loc src_op =
  match src_op with
  | Operand.StringLiteral _ ->
      Asm.Instruction.Lea (dest_loc, emit_oper regalloc data_section src_op)
  | _ -> Mov (dest_loc, emit_oper regalloc data_section src_op)

(** [emit_save_registers text_section data_section registers] emits instructions
    into [text_section data_section] to save [registers] on the stack. If the
    register contents are to be restored, they must be restored using
    [emit_restore_registers]. *)
let emit_save_registers text_section registers =
  let open Asm.Instruction in
  let push_instructions = List.map (fun reg -> Push (Register reg)) registers in
  let added_size = var_size * List.length registers in
  let extra_offset = align_offset added_size in
  Asm.Section.add_all text_section push_instructions;
  Asm.Section.add text_section (Sub (Register RSP, Intermediate extra_offset))

(** [emit_save_registers text_section data_section registers] emits instructions
    into [text_section data_section] to pop [registers] from the stack. The
    registers being restored must be the same as those most recently pushed
    using [emit_save_registers]. *)
let emit_restore_registers text_section registers =
  let open Asm.Instruction in
  let pop_instructions =
    List.rev_map (fun reg -> Pop (Register reg)) registers
  in
  let added_size = var_size * List.length registers in
  let extra_offset = align_offset added_size in
  Asm.Section.add text_section (Add (Register RSP, Intermediate extra_offset));
  Asm.Section.add_all text_section pop_instructions

let emit_call text_section data_section regalloc name args return_loc_opt =
  let save_registers =
    List.filter
      (fun reg ->
        match return_loc_opt with
        | Some (Asm.Operand.Register return_reg) -> reg <> return_reg
        | _ -> true)
      Asm.Register.caller_saved_data_registers
  in
  emit_save_registers text_section save_registers;
  let param_moves =
    Util.zip_shortest args Asm.Register.parameter_registers
    |> List.map (fun (arg, reg) ->
           make_mov regalloc data_section (Register reg) arg)
  in
  Asm.Section.add_all text_section param_moves;
  Asm.Section.add text_section (Asm.Instruction.Call (Label name));
  (match return_loc_opt with
  | Some return_loc ->
      Asm.Section.add text_section (Mov (return_loc, Register RAX))
  | None -> ());
  emit_restore_registers text_section save_registers

let emit_ir text_section data_section regalloc = function
  | Ir.Assign (var, op) ->
      Asm.Section.add text_section
        (make_mov regalloc data_section (emit_var regalloc var) op)
  | Add (var, op, op2) ->
      Asm.Section.add_all text_section
        [
          Mov (emit_var regalloc var, emit_oper regalloc data_section op);
          Add (emit_var regalloc var, emit_oper regalloc data_section op2);
        ]
  | Sub (var, op, op2) | TestEqual (var, op, op2) ->
      Asm.Section.add_all text_section
        [
          Mov (emit_var regalloc var, emit_oper regalloc data_section op);
          Sub (emit_var regalloc var, emit_oper regalloc data_section op2);
        ]
  | Mul (var, op, op2) ->
      Asm.Section.add_all text_section
        [
          Mov (emit_var regalloc var, emit_oper regalloc data_section op);
          IMul (emit_var regalloc var, emit_oper regalloc data_section op2);
        ]
  | Ref _ -> failwith "ref not impl"
  | Deref _ -> failwith "deref not impl"
  | DebugPrint op ->
      emit_call text_section data_section regalloc debug_print_int_symbol [ op ]
        None
  | Call (var, name, args) ->
      emit_call text_section data_section regalloc (mangle name) args
        (Some (emit_var regalloc var))
  | GetParam var -> (
      let param_passing = ParameterPassingContext.make () in
      match ParameterPassingContext.get_next param_passing with
      | Register dest ->
          Asm.Section.add text_section
            (Mov (emit_var regalloc var, Register dest))
      | Spill _ -> failwith "todo")
  | Return op_opt ->
      Option.map
        (fun op ->
          Asm.Section.add text_section
            (Mov (Register RAX, emit_oper regalloc data_section op)))
        op_opt
      |> ignore;
      emit_restore_registers text_section
        Asm.Register.callee_saved_data_registers;
      Asm.Section.add_all text_section
        [ Mov (Register RSP, Register RBP); Pop (Register RBP); Ret ]

let emit_bb text_section data_section cfg regalloc bb =
  Asm.Section.add text_section
    (Label
       (Asm.Label.make ~is_global:false ~is_external:false
          (Basic_block.label_for bb)));
  bb |> Basic_block.to_list
  |> List.iter (emit_ir text_section data_section regalloc);
  match Basic_block.condition_of bb with
  | Never | Conditional (Constant 0) -> ()
  | Always | Conditional (Constant _) ->
      let dest_bb = Cfg.take_branch cfg bb true |> Option.get in
      Asm.Section.add text_section (Jmp (Label (Basic_block.label_for dest_bb)))
  | Conditional op -> (
      let true_bb = Cfg.take_branch cfg bb true |> Option.get in
      let false_bb = Cfg.take_branch cfg bb false |> Option.get in
      match op with
      | Variable var ->
          Asm.Section.add text_section
            (Cmp (emit_var regalloc var, Intermediate 0));
          Asm.Section.add text_section
            (Je (Label (Basic_block.label_for true_bb)));
          Asm.Section.add text_section
            (Jmp (Label (Basic_block.label_for false_bb)))
      | Constant _ | StringLiteral _ -> failwith "failure")

let emit_preamble ~text_section ~data_section:_ ffi_names decl_names =
  Asm.Section.add_all text_section
    (List.map
       (fun ffi_name ->
         Asm.Instruction.Label
           (Asm.Label.make ~is_global:false ~is_external:true
              (mangle_ffi ffi_name)))
       ffi_names);
  Asm.Section.add_all text_section
    (List.map
       (fun decl_name ->
         Asm.Instruction.Label
           (Asm.Label.make ~is_global:false ~is_external:true (mangle decl_name)))
       decl_names);
  Asm.Section.add text_section
    (Label
       (Asm.Label.make ~is_global:false ~is_external:true debug_print_int_symbol))

let emit_cfg ~text_section ~data_section cfg regalloc =
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
  Asm.Section.add text_section
    (Label
       (Asm.Label.make ~is_global:true ~is_external:false
          (mangle (Cfg.name_of cfg))));
  (* no need to align here, we can assume as a callee that 8 bytes for the
     return address was already pushed to the stack. *)
  Asm.Section.add_all text_section
    [
      Push (Register RBP);
      Mov (Register RBP, Register RSP);
      Sub (Register RSP, Intermediate stack_bytes);
    ];
  (* restore is done at returns *)
  emit_save_registers text_section Asm.Register.callee_saved_data_registers;
  Asm.Section.add text_section (Jmp (Label (Basic_block.label_for entry)));
  Cfg.blocks_of cfg
  |> List.iter (emit_bb text_section data_section cfg regalloc)
