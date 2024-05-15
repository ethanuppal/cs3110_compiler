(* TODO: standardize instruction id? *)
type instr_id = Id.t * int

(** [start] is the first instruction (in terms of the arbitrary ordering)
    *after* which a variable is live. [stop] is the last instruction *after*
    which a variable is live.

    See definition on page 898: https://dl.acm.org/doi/pdf/10.1145/330249.330250 *)
type interval = {
  start : instr_id;
  stop : instr_id;
}

type allocation =
  | Register of Asm.Register.t
  | Spill of int

module BBAnalysis = Liveliness.BasicBlockAnalysis

let live_intervals (cfg : Cfg.t) (liveliness : BBAnalysis.t IdMap.t)
    (ordering : InstrOrdering.t) =
  let tbl = VariableMap.create 16 in

  let expand_interval original live_id =
    let cmp = InstrOrdering.compare ordering in
    if cmp live_id original.start < 0 then { original with start = live_id }
    else if cmp live_id original.stop > 0 then { original with stop = live_id }
    else original
  in

  let update_table instr_id used_set =
    Liveliness.VariableSet.iter
      (fun live ->
        let current_opt = VariableMap.find_opt tbl live in
        let new_interval =
          match current_opt with
          | None -> { start = instr_id; stop = instr_id }
          | Some current -> expand_interval current instr_id
        in
        VariableMap.replace tbl live new_interval)
      used_set
  in

  Cfg.iter
    (fun bb ->
      let bb_id = Basic_block.id_of bb in
      let analysis = IdMap.find liveliness bb_id in
      for instr_idx = 0 to Basic_block.length_of bb - 1 do
        let live_set = BBAnalysis.live_before_instr analysis instr_idx in
        let kill_var = Basic_block.get_ir bb instr_idx |> Ir.kill_of in
        let used_set =
          match kill_var with
          | Some var -> Liveliness.VariableSet.add var live_set
          | None -> live_set
        in
        update_table (bb_id, instr_idx) used_set
      done)
    cfg;

  VariableMap.to_seq tbl |> List.of_seq

(* Algorithm source:
   https://en.wikipedia.org/wiki/Register_allocation#Pseudocode *)
let linear_scan intervals ordering registers =
  let compare_instr_id = InstrOrdering.compare ordering in
  let compare_pair_start (_, i1) (_, i2) = compare_instr_id i1.start i2.start in
  let compare_pair_end (_, i1) (_, i2) = compare_instr_id i1.stop i2.stop in
  let sorted_intervals = List.sort compare_pair_start intervals in

  let assigned_alloc : allocation VariableMap.t = VariableMap.create 4 in

  let module RegSet = Set.Make (Asm.Register) in
  let free_registers : RegSet.t ref = ref (RegSet.of_list registers) in

  (* must remain sorted by increasing end point *)
  let active : (Variable.t * interval) BatRefList.t = BatRefList.empty () in

  let cur_loc = ref 0 in
  let next_spill_loc () =
    let result = !cur_loc in
    cur_loc := !cur_loc + 1;
    result
  in

  let expire_old_intervals (current : interval) =
    (* this is also really annoying because BatRefList has no partition *)
    BatRefList.filter
      (fun (var, interval) ->
        let keep = compare_instr_id interval.stop current.start >= 0 in
        (if not keep then
           let alloc = VariableMap.find assigned_alloc var in
           match alloc with
           | Register r -> free_registers := RegSet.add r !free_registers
           | Spill _ -> failwith "Interval in active cannot be spilled");
        keep)
      active
  in

  let spill_at_interval ((var, interval) : Variable.t * interval) =
    let spill_var, spill_interval = BatRefList.last active in

    if compare_instr_id spill_interval.stop interval.stop > 0 then (
      (* spill guaranteed to be assigned an actual register *)
      let alloc = VariableMap.find assigned_alloc spill_var in
      assert (
        match alloc with
        | Spill _ -> false
        | _ -> true);

      VariableMap.replace assigned_alloc var alloc;
      VariableMap.replace assigned_alloc spill_var (Spill (next_spill_loc ()));

      (* this sucks. can we maybe keep active in reverse order? *)
      BatRefList.Index.remove_at active (BatRefList.length active - 1);

      (* add_sort is buggy... TODO: new impl *)
      BatRefList.push active (var, interval);
      BatRefList.sort ~cmp:compare_pair_end active)
    else VariableMap.replace assigned_alloc var (Spill (next_spill_loc ()))
  in

  List.iter
    (fun (var, interval) ->
      expire_old_intervals interval;
      match RegSet.choose_opt !free_registers with
      | Some register ->
          free_registers := RegSet.remove register !free_registers;
          VariableMap.replace assigned_alloc var (Register register);
          BatRefList.push active (var, interval);
          BatRefList.sort ~cmp:compare_pair_end active
      | None -> spill_at_interval (var, interval))
    sorted_intervals;

  assigned_alloc

let allocate_for cfg registers liveliness ordering =
  let vars_with_intervals = live_intervals cfg liveliness ordering in
  linear_scan vars_with_intervals ordering registers
