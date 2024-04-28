open Util
module VariableSet = Set.Make (Variable)

module Analysis = struct
  type t = {
    mutable live_in : VariableSet.t;
    mutable live_out : VariableSet.t;
    gen : VariableSet.t;
    kill : VariableSet.t;
  }

  let make ~gen ~kill =
    { live_in = VariableSet.empty; live_out = VariableSet.empty; gen; kill }

  let live_in analysis = analysis.live_in
  let live_out analysis = analysis.live_out

  let to_string analysis =
    let set_to_string set =
      VariableSet.elements set
      |> List.map Variable.to_string
      |> String.concat ", "
    in
    Printf.sprintf
      "{ live_in = [%s]; live_out = [%s]; gen = [%s]; kill = [%s] }"
      (set_to_string analysis.live_in)
      (set_to_string analysis.live_out)
      (set_to_string analysis.gen)
      (set_to_string analysis.kill)
end

(** [initial_analysis_of bb] is the gen and kill sets of [bb] and empty live-in
    and live-out sets. *)
let initial_analysis_of bb =
  let open Ir in
  let ir_list = Basic_block.to_list bb in
  let gen = ref VariableSet.empty in
  let kill = ref VariableSet.empty in
  let add_gen var =
    if not (VariableSet.mem var !kill) then gen := VariableSet.add var !gen
  in
  let add_kill var = kill := VariableSet.add var !kill in
  List.iter
    (fun ir ->
      match ir with
      | Assign (var, Operand.Variable oper)
      | Ref (var, Operand.Variable oper)
      | Deref (var, Operand.Variable oper) ->
          add_kill var;
          add_gen oper
      | Add (var, oper1, oper2)
      | Sub (var, oper1, oper2)
      | TestEqual (var, oper1, oper2) ->
          add_kill var;
          Operand.var_of_opt oper1 |> Option.map add_gen |> ignore;
          Operand.var_of_opt oper2 |> Option.map add_gen |> ignore
      | _ -> ())
    ir_list;
  Analysis.make ~gen:!gen ~kill:!kill

(* MAJOR ISSUE NEED TO WORK BACKWARD FROM END OF FUNCTION TODO: TALK TO UTKU *)
let analysis_of bb_list =
  let liveliness = IdMap.create 16 in
  List.iter
    (fun bb ->
      IdMap.replace liveliness (Basic_block.id_of bb) (initial_analysis_of bb))
    bb_list;
  (* let work_list = Queue.create () in while *)
  liveliness
