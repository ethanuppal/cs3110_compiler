type sim_value = (int, string) Either.t

type t = {
  context : sim_value Context.t;
  mutable current : Cfg.t option;
  mutable args : sim_value list list;
  mutable return_value : sim_value;
  mutable output : string;
}

let make () =
  {
    context = Context.make ();
    current = None;
    args = [];
    return_value = Left 0;
    output = "";
  }

let find_cfg_by_name cfgs name =
  List.find (fun cfg -> Cfg.name_of cfg = name) cfgs

let rec run_cfg simulator cfgs cfg =
  let entry = Cfg.entry_to cfg in
  let eval = function
    | Operand.Variable var ->
        Context.get simulator.context (Variable.to_string var) |> Option.get
    | Operand.Constant int -> Left int
    | Operand.StringLiteral value -> Right (StringLiteral.value_of value)
  in
  let eval_int op =
    match eval op with
    | Left int -> int
    | Right _ -> failwith "Ir_sim: attempt to access string as int"
  in
  (* let eval_str op = match eval op with | Left _ -> failwith "Ir_sim: attempt
     to access int as string" | Right str -> str in *)
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
                   (Left (eval_int oper1 + eval_int oper2));
                 false
             | Ir.Sub (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (Left (eval_int oper1 - eval_int oper2));
                 false
             | Ir.Mul (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (Left (eval_int oper1 * eval_int oper2));
                 false
             | Ir.Shl (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (Left (eval_int oper1 lsl eval_int oper2));
                 false
             | Ir.Shr (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (Left (eval_int oper1 lsr eval_int oper2));
                 false
             | Ir.Sar (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (Left (eval_int oper1 asr eval_int oper2));
                 false
             | Ir.TestEqual (var, oper1, oper2) ->
                 Context.insert simulator.context (Variable.to_string var)
                   (if eval oper1 = eval oper2 then Left 1 else Left 0);
                 false
             | Ir.Ref _ | Ir.Deref _ ->
                 failwith "Ir_sim does not support pointers"
             | Ir.DebugPrint oper ->
                 simulator.output <-
                   simulator.output
                   ^ Printf.sprintf "%s\n"
                       (eval oper
                       |> Either.fold ~left:string_of_int ~right:Fun.id);
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
        | Conditional oper -> Some (eval_int oper <> 0)
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
