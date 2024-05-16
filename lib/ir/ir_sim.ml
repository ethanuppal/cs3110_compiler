type t = {
  context : int Context.t;
  mutable current : Cfg.t option;
  mutable args : int list list;
  mutable return_value : int;
  mutable output : string;
}

let make () =
  {
    context = Context.make ();
    current = None;
    args = [];
    return_value = 0;
    output = "";
  }

let find_cfg_by_name cfgs name =
  List.find (fun cfg -> Cfg.name_of cfg = name) cfgs

let rec run_cfg simulator cfgs cfg =
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
             | Ir.Mul (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (eval oper1 * eval oper2);
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
             | Ir.Call (result, name, args) ->
                 let called_cfg = find_cfg_by_name cfgs name in
                 simulator.args <- List.map eval args :: simulator.args;
                 run_cfg simulator cfgs called_cfg;
                 simulator.args <- List.tl simulator.args;
                 Context.insert simulator.context
                   (Variable.to_string result)
                   simulator.return_value;
                 false
             | Ir.GetParam var ->
                 let args = List.hd simulator.args in
                 Context.insert simulator.context (Variable.to_string var)
                   (List.hd args);
                 simulator.args <- List.tl args :: List.tl simulator.args;
                 false
             | Ir.Return oper_opt ->
                 (match oper_opt with
                 | Some oper -> simulator.return_value <- eval oper
                 | None -> ());
                 true)
           false
    in
    if not should_exit then
      let cond_opt =
        match Basic_block.condition_of bb with
        | Always -> Some true
        | Never -> None
        | Conditional oper -> Some (eval oper <> 0)
      in
      match cond_opt with
      | Some cond -> (
          match Cfg.take_branch cfg bb cond with
          | Some bb2 -> run_aux bb2
          | None -> ())
      | None -> ()
  in
  run_aux entry

let run simulator cfgs =
  Context.push simulator.context;
  let main_cfg = find_cfg_by_name cfgs [ "main" ] in
  run_cfg simulator cfgs main_cfg;
  Context.pop simulator.context

let output_of simulator = simulator.output
let clear_output simulator = simulator.output <- ""
