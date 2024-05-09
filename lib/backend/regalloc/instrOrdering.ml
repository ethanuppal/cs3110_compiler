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
      OrderMap.add map (Basic_block.id_of bb) !n;
      n := !n + 1)
    cfg;
  map

let compare ordering id1 id2 =
  let bb_id_1, instr_idx_1 = id1 in
  let bb_id_2, instr_idx_2 = id2 in
  let bb_order_1 = OrderMap.find bb_id_1 ordering in
  let bb_order_2 = OrderMap.find bb_id_2 ordering in
  Stdlib.compare (bb_order_1, instr_idx_1) (bb_order_2, instr_idx_2)
