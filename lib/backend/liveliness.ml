open Util
module VariableSet = Set.Make (Variable)

type analysis = {
  mutable live_in : VariableSet.t;
  mutable live_out : VariableSet.t;
  gen : VariableSet.t;
  kill : VariableSet.t;
}

module Analysis = struct
  type t = analysis

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
      | Assign (var, oper) | Ref (var, oper) | Deref (var, oper) ->
          add_kill var;
          Operand.var_of_opt oper |> Option.map add_gen |> ignore
      | Add (var, oper1, oper2)
      | Sub (var, oper1, oper2)
      | TestEqual (var, oper1, oper2) ->
          add_kill var;
          Operand.var_of_opt oper1 |> Option.map add_gen |> ignore;
          Operand.var_of_opt oper2 |> Option.map add_gen |> ignore
      | _ -> ())
    ir_list;
  Analysis.make ~gen:!gen ~kill:!kill

(** [pass work_list liveliness cfg bb] performs a single pass of liveliness
    analysis on a basic block *)
let pass work_list liveliness cfg bb =
  let analysis = IdMap.find liveliness (Basic_block.id_of bb) in
  let live_in_old = analysis.live_in in
  analysis.live_in <-
    VariableSet.union analysis.gen
      (VariableSet.diff analysis.live_out analysis.kill);
  analysis.live_out <-
    Cfg.out_edges cfg bb
    |> List.fold_left
         (fun acc (succ, _) ->
           let succ_analysis = IdMap.find liveliness (Basic_block.id_of succ) in
           VariableSet.union acc succ_analysis.live_in)
         VariableSet.empty;
  if analysis.live_in <> live_in_old then (
    List.iter (fun (bb, _) -> Queue.add bb work_list) (Cfg.in_edges cfg bb);
    true)
  else false

(** [iterate liveliness cfg] performs an iteration of liveliness analysis on
    [cfg], updating partial results in [liveliness], and returiing whether any
    changes were made. *)
let iterate liveliness cfg =
  let work_list = Queue.create () in
  List.iter (fun bb -> Queue.add bb work_list) (Cfg.exit_points cfg);
  let result = ref false in
  while not (Queue.is_empty work_list) do
    let top_bb = Queue.take work_list in
    result := !result || pass work_list liveliness cfg top_bb
  done;
  !result

let analysis_of cfg =
  let liveliness = IdMap.create 16 in
  Cfg.iter
    (fun bb ->
      IdMap.add liveliness (Basic_block.id_of bb) (initial_analysis_of bb))
    cfg;
  let rec converge () = if iterate liveliness cfg then converge () in
  converge ();
  liveliness
