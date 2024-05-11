type t = {
  context : int Context.t;
  mutable current : Cfg.t option;
  mutable output : string;
}

let make () = { context = Context.make (); current = None; output = "" }

let find_cfg_by_name cfgs name =
  List.find (fun cfg -> Cfg.name_of cfg = name) cfgs

let rec run_cfg simulator cfgs cfg =
  Context.push simulator.context;
  let entry = Cfg.entry_to cfg in
  let eval = function
    | Operand.Variable var ->
        Context.get simulator.context (Variable.to_string var) |> Option.get
    | Operand.Constant int -> int
  in
  let rec run_aux bb =
    let should_exit =
      Basic_block.to_list bb
      |> List.fold_left
           (fun acc ir ->
             acc
             ||
             match ir with
             | Ir.Assign (var, oper) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (eval oper);
                 false
             | Ir.Add (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (eval oper1 + eval oper2);
                 false
             | Ir.Sub (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (eval oper1 - eval oper2);
                 false
             | Ir.TestEqual (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (if eval oper1 = eval oper2 then 1 else 0);
                 false
             | Ir.Ref _ | Ir.Deref _ ->
                 failwith "Ir_sim does not support pointers"
             | Ir.DebugPrint oper ->
                 simulator.output <-
                   simulator.output ^ Printf.sprintf "%d\n" (eval oper);
                 false
             | Ir.Call (_, name, _) ->
                 let called_cfg = find_cfg_by_name cfgs name in
                 run_cfg simulator cfgs called_cfg;
                 false
             | Ir.Return _ -> true)
           false
    in
    if not should_exit then
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
  run_aux entry;
  Context.pop simulator.context

let run simulator cfgs =
  let main_cfg = find_cfg_by_name cfgs "main" in
  run_cfg simulator cfgs main_cfg

let output_of simulator = simulator.output
let clear_output simulator = simulator.output <- ""
