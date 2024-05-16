module OrderMap = Hashtbl.Make (Id)

type t = int OrderMap.t
type instr_id = Id.t * int

let make cfg =
  (* arbitrary order for now, can be improved later. does not actually matter,
     any ordering works. better orderings only improve performance. *)
  let map = OrderMap.create 10 in
  let n = ref 0 in
  Cfg.iter
    (fun bb ->
      OrderMap.add map (BasicBlock.id_of bb) !n;
      n := !n + 1)
    cfg;
  map

let compare (ordering : t) (bb_id_1, instr_idx_1) (bb_id_2, instr_idx_2) =
  let bb_order_1 = OrderMap.find ordering bb_id_1 in
  let bb_order_2 = OrderMap.find ordering bb_id_2 in
  if bb_order_1 > bb_order_2 then 1
  else if bb_order_1 = bb_order_2 then
    if instr_idx_1 > instr_idx_2 then 1
    else if instr_idx_1 = instr_idx_2 then 0
    else -1
  else -1
