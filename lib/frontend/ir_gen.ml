open Ast

exception UnboundVariable of { name : string }

(* TODO: what are the invariants between context and cfg? should they be
   packaged together? At least one: variables in ctx must be in cfg *)

let get_or_else excn opt =
  match opt with
  | Some v -> v
  | None -> raise excn

let rec generate_expr ctx cfg block expr =
  match expr with
  | Var { name; _ } ->
      let var_opt = Context.get ctx name in
      let var = get_or_else (UnboundVariable { name }) var_opt in
      Operand.make_var var
  | ConstInt value -> Operand.make_const value
  | ConstBool value -> Operand.make_const (if value then 1 else 0)
  | Infix { lhs; op; rhs; _ } ->
      let result = Variable.make () in
      let lhs_result = generate_expr ctx cfg block lhs in
      let rhs_result = generate_expr ctx cfg block rhs in
      let ir_instr =
        match op with
        | Plus -> Ir.Add (result, lhs_result, rhs_result)
        | Minus -> Ir.Sub (result, lhs_result, rhs_result)
        | Equals -> Ir.TestEqual (result, lhs_result, rhs_result)
        | _ -> failwith "not implemented"
      in
      Basic_block.add_ir block ir_instr;
      Operand.make_var result
  | Prefix { op; rhs; _ } ->
      let result = Variable.make () in
      let rhs_result = generate_expr ctx cfg block rhs in
      let ir_instr =
        match op with
        | Plus -> Ir.Assign (result, rhs_result)
        | Minus -> Ir.Sub (result, Operand.make_const 0, rhs_result)
        | Times -> Ir.Deref (result, rhs_result)
        | BitAnd -> Ir.Ref (result, rhs_result)
        | _ -> failwith "not implemented"
      in
      Basic_block.add_ir block ir_instr;
      Operand.make_var result

(** [generate_stmt ctx cfg block stmt] adds IR for [stmt] (and potentially more
    blocks) onto [block] in [cfg], and returns the block that program flow
    should continue from. *)
let rec generate_stmt ctx cfg block = function
  | Call _ -> failwith "not implemented"
  | Declaration { expr; name; _ } ->
      (* IR for this could probably be improved but it's fine *)
      let result = generate_expr ctx cfg block expr in
      let result_var = Variable.make () in
      let assign = Ir.Assign (result_var, result) in
      Basic_block.add_ir block assign;
      Context.insert ctx name result_var;
      block
  | Assignment (name, expr) ->
      let result = generate_expr ctx cfg block expr in
      let result_var =
        Context.get ctx name |> get_or_else (UnboundVariable { name })
      in
      let assign = Ir.Assign (result_var, result) in
      Basic_block.add_ir block assign;
      block
  | If { cond; body } ->
      let cond_result = generate_expr ctx cfg block cond in
      let bt = Cfg.create_block cfg in
      let bf = Cfg.create_block cfg in
      let cond = Branch_condition.Conditional cond_result in
      Cfg.insert_branch cfg block cond bt bf;

      (* True case *)
      Context.push ctx;
      let true_end_block = generate_stmt_lst ctx cfg bt body in
      Context.pop ctx;
      Cfg.insert_unconditional cfg true_end_block bf;

      bf
  | Function _ -> failwith "not allowed"
  | Print expr ->
      let to_print = generate_expr ctx cfg block expr in
      Basic_block.add_ir block (Ir.DebugPrint to_print);
      block

and generate_stmt_lst ctx cfg block lst =
  let block_ref = ref block in
  List.iter (fun stmt -> block_ref := generate_stmt ctx cfg !block_ref stmt) lst;
  !block_ref

let generate prog =
  match prog with
  | Function { name = "main"; body } :: _ ->
      let ctx = Context.make () in
      Context.push ctx;
      let cfg = Cfg.make "main" in
      ignore (generate_stmt_lst ctx cfg (Cfg.entry_to cfg) body);
      [ cfg ]
  | _ -> failwith "not implemented"
