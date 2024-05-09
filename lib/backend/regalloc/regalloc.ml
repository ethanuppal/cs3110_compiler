open Util
module VarMap = Map.Make (Variable)

type var_reg_map = Asm.Register.t VarMap.t

(* TODO: standardize instruction id? *)
type instr_id = Id.t * int

(** [start] is the first instruction (in terms of the arbitrary ordering)
    *after* which a variable is live. [stop] is the last instruction *after*
    which a variable is live. *)
type interval = {
  start : instr_id;
  stop : instr_id;
}

module BBAnalysis = Liveliness.BasicBlockAnalysis

let live_intervals (cfg : Cfg.t) (liveliness : BBAnalysis.t IdMap.t)
    (ordering : InstrOrdering.t) =
  let module VarTbl = Hashtbl.Make (Variable) in
  let tbl = VarTbl.create 10 in

  let expand_interval original live_id =
    let cmp = InstrOrdering.compare ordering in
    if cmp live_id original.start < 0 then { original with start = live_id }
    else if cmp live_id original.stop > 0 then { original with stop = live_id }
    else original
  in

  let update_table instr_id live_set =
    Liveliness.VariableSet.iter
      (fun live ->
        let current_opt = VarTbl.find_opt tbl live in
        let new_interval =
          match current_opt with
          | None -> { start = instr_id; stop = instr_id }
          | Some current -> expand_interval current instr_id
        in
        VarTbl.replace tbl live new_interval)
      live_set
  in

  Cfg.iter
    (fun bb ->
      let bb_id = Basic_block.id_of bb in
      let analysis = IdMap.find liveliness bb_id in
      for instr_idx = 0 to Basic_block.length_of bb - 1 do
        let live_set = BBAnalysis.live_after_instr analysis instr_idx in
        update_table (bb_id, instr_idx) live_set
      done)
    cfg;

  VarTbl.to_seq tbl |> List.of_seq

(* for now we're going to construct liveliness and ordering in here--dependency
   can be injected later if necessary *)
let allocate_for cfg =
  let liveliness = Liveliness.analysis_of cfg in
  let ordering = InstrOrdering.make cfg in
  let _intervals = live_intervals cfg liveliness ordering in
  VarMap.empty
