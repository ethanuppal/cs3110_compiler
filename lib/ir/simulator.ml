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
           | Ir.Assign (var, operand) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval operand)
           | Ir.Add (var, operand1, operand2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval operand1 + eval operand2)
           | Ir.Sub (var, operand1, operand2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (eval operand1 - eval operand2)
           | Ir.TestEqual (var, operand1, operand2) ->
               Context.insert simulator.context (Variable.to_string var)
                 (if eval operand1 = eval operand2 then 1 else 0)
           | Ir.DebugPrint operand ->
               simulator.output <-
                 simulator.output ^ Printf.sprintf "%d\n" (eval operand)
           | _ -> failwith "Simulator.run doesn't implement all of IR yet");
    let cond =
      match Basic_block.condition_of bb with
      | Always -> true
      | Never -> false
      | Conditional operand -> eval operand <> 0
    in
    match Cfg.take_branch cfg bb cond with
    | Some bb2 -> run_aux bb2
    | None -> ()
  in
  run_aux entry

let dump simulator =
  Printf.sprintf "IR Simulation:\n"
  ^ (Context.to_seq simulator.context
    |> Seq.map (fun (var, value) -> Printf.sprintf "  %s = %d" var value)
    |> List.of_seq
    |> List.sort_uniq String.compare
    |> String.concat "\n")
  ^ "\n"

let output_of simulator = simulator.output
