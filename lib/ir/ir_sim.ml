type t = {
  context : int Context.t;
  mutable output : string;
}

let make () = { context = Context.make (); output = "" }

let run simulator cfg =
  Context.push simulator.context;
  let entry = Cfg.entry_to cfg in
  let eval = function
    | Operand.Variable var ->
        Context.get simulator.context (Variable.to_string var) |> Option.get
    | Operand.Constant int -> int
  in
  let rec run_aux bb =
    Basic_block.to_list bb
    |> List.iter (fun ir ->
           match ir with
           | Ir.Assign (var, oper) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval oper)
           | Ir.Add (var, oper1, oper2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval oper1 + eval oper2)
           | Ir.Sub (var, oper1, oper2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval oper1 - eval oper2)
           | Ir.TestEqual (var, oper1, oper2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (if eval oper1 = eval oper2 then 1 else 0)
           | Ir.Ref _ | Ir.Deref _ ->
               failwith "Ir_sim does not support pointers"
           | Ir.DebugPrint oper ->
               simulator.output <-
                 simulator.output ^ Printf.sprintf "%d\n" (eval oper));
    let cond =
      match Basic_block.condition_of bb with
      | Always -> true
      | Never -> false
      | Conditional oper -> eval oper <> 0
    in
    match Cfg.take_branch cfg bb cond with
    | Some bb2 -> run_aux bb2
    | None -> ()
  in
  run_aux entry

let output_of simulator = simulator.output
