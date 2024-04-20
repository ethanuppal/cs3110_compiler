open Ast

(** [generate_stmt cfg block stmt] adds IR for [stmt] (and potentially more
    blocks) onto [block] in [cfg]. *)
let generate_stmt cfg block = function
  | If _ -> failwith "not implemented"
  | Call _ -> failwith "not implemented"
  | Declaration { name; expr } ->
      let cfg = Cfg.make () in
      let var = Variable.make () in
      let expr_bb, expr_var = generate_expr expr in
      Basic_block.add bb (Ir.Assign (var, Operand.make_const 0))
  | Assignment _ -> ()
  | Function _ -> failwith "not allowed"
  | Print _ -> failwith "not implemented"

let generate [ Function { name = "main"; _ } ] = [ Cfg.make () ]
