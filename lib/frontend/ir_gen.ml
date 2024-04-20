open Ast

type var_context = Variable.t Context.t

(* TODO: result types? *)
exception UnboundVariable of { name : string }

let generate_expr (ctx : var_context) (_cfg : Cfg.t) (_block : Cfg.Block.t)
    (expr : Ast.expr) : Operand.t =
  match expr with
  | Var { name; _ } -> (
      let var_opt = Context.get ctx name in
      match var_opt with
      | Some var -> Operand.make_var var
      | None -> raise (UnboundVariable { name }))
  | ConstInt value -> Operand.make_const value
  | Infix { lhs; op; rhs; _ } -> failwith "hi"
  | _ -> failwith "not implemented"

(** [generate_stmt ctx cfg block stmt] adds IR for [stmt] (and potentially more
    blocks) onto [block] in [cfg], and returns the block that program flow
    should continue from. *)
let generate_stmt ctx cfg block = function
  | If _ -> failwith "not implemented"
  | Call _ -> failwith "not implemented"
  | Declaration { expr; _ } ->
      let _ = generate_expr ctx cfg block expr in
      block
  | Assignment _ -> failwith "not implemented"
  | Function _ -> failwith "not allowed"
  | Print _ -> failwith "not implemented"

let generate prog =
  match prog with
  | Function { name = "main"; body } :: _ ->
      let ctx = Context.make () in
      let cfg = Cfg.make () in
      let block = ref (Cfg.entry cfg) in
      List.iter (fun stmt -> block := generate_stmt ctx cfg !block stmt) body;
      [ cfg ]
  | _ -> failwith "not implemented"
