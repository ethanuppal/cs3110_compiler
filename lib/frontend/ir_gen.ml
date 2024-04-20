open Ast

let var_gen = Id.Gen.make ()

(* gives back basic block and variable storing final result *)
let generate_expr (expr : Ast.expr) =
  ignore expr;
  ( Basic_block.make Branch_condition.Unconditional,
    Variable.make (Id.Gen.next var_gen) )

let generate_stmt = function
  | If _ -> failwith "not implemented"
  | Call _ -> failwith "not implemented"
  | Declaration { name; expr } ->
      let cfg = Cfg.make () in
      let var = Variable.make (Id.Gen.next var_gen) in
      let expr_bb, expr_var = generate_expr expr in
      Basic_block.add bb (Ir.Assign (var, Operand.make_const 0))
  | Assignment _ -> ()
  | Function _ -> failwith "not allowed"
  | Print _ -> failwith "not implemented"

let generate [ Function { name = "main"; _ } ] = [ Cfg.make () ]
