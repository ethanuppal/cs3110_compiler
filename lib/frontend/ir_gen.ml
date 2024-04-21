open Ast

type var_context = Variable.t Context.t

(* TODO: result types? *)
exception UnboundVariable of { name : string }

(* TODO: what are the invariants between context and cfg? should they be
   packaged together? At least one: variables in ctx must be in cfg *)

let get_or_else excn opt =
  match opt with
  | Some v -> v
  | None -> raise excn

let rec generate_expr (ctx : var_context) (cfg : Cfg.t) (block : Basic_block.t)
    (expr : Ast.expr) : Operand.t =
  match expr with
  | Var { name; _ } ->
      let var_opt = Context.get ctx name in
      let var = get_or_else (UnboundVariable { name }) var_opt in
      Operand.make_var var
  | ConstInt value -> Operand.make_const value
  | Infix { lhs; op; rhs; _ } -> (
      let result = Variable.make () in
      let lhs_result = generate_expr ctx cfg block lhs in
      let rhs_result = generate_expr ctx cfg block rhs in
      match op with
      | Plus ->
          let ir = Ir.Add (result, lhs_result, rhs_result) in
          Basic_block.add_ir block ir;
          Operand.make_var result
      | _ -> failwith "not implemented")
  | _ -> failwith "not implemented"

(** [generate_stmt ctx cfg block stmt] adds IR for [stmt] (and potentially more
    blocks) onto [block] in [cfg], and returns the block that program flow
    should continue from. *)
let generate_stmt ctx cfg block = function
  | If _ -> failwith "not implemented"
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
  | Function _ -> failwith "not allowed"
  | Print _ -> failwith "not implemented"

let generate prog =
  match prog with
  | Function { name = "main"; body } :: _ ->
      let ctx = Context.make () in
      Context.push ctx;
      let cfg = Cfg.make () in
      let block = ref (Cfg.entry cfg) in
      List.iter (fun stmt -> block := generate_stmt ctx cfg !block stmt) body;
      [ cfg ]
  | _ -> failwith "not implemented"
