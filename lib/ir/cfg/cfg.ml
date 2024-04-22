module Graph = Digraph.Make (Basic_block)

(** RI: [entry] is in [graph] and has no in neighbors. A block in [graph] must
    have zero out neighbors if its condition is [Never], one if its condition is
    [Always], and two if its condition is [Conditional]. *)
type t = {
  name : string;
  entry : Basic_block.t;
  graph : bool Graph.t;
}

let rep_ok cfg =
  if Rep_ok.check then (
    let vertices = Graph.vertices_of cfg.graph in
    if
      List.find_opt
        (fun bb -> Basic_block.id_of bb = Basic_block.id_of cfg.entry)
        vertices
      = None
    then failwith "rep_ok";
    List.iter
      (fun bb ->
        let out_degree = Graph.out_neighbors cfg.graph bb |> List.length in
        match Basic_block.condition_of bb with
        | Never -> if out_degree <> 0 then failwith "rep_ok"
        | Always -> if out_degree <> 1 then failwith "rep_ok"
        | Conditional _ -> if out_degree <> 2 then failwith "rep_ok")
      vertices);
  cfg

let make name =
  let graph = Graph.empty () in
  let entry = Basic_block.make () in
  Graph.add_vertex graph entry;
  { name; entry; graph } |> rep_ok

let name_of { name; _ } = name
let entry_to { entry; _ } = entry

let create_block cfg =
  let cfg = rep_ok cfg in
  let block = Basic_block.make () in
  Graph.add_vertex cfg.graph block;
  block

let insert_branch cfg block cond bt bf =
  let cfg = rep_ok cfg in
  assert (Basic_block.condition_of block = Never);
  Graph.add_edge cfg.graph block true bt;
  Graph.add_edge cfg.graph block false bf;
  Basic_block.set_condition block cond

let insert_unconditional cfg pred succ =
  let cfg = rep_ok cfg in
  assert (Basic_block.condition_of pred = Never);
  Graph.add_edge cfg.graph pred true succ;
  Basic_block.set_condition pred Branch_condition.Always

let take_branch cfg bb cond =
  let cfg = rep_ok cfg in
  let out = Graph.out_neighbors cfg.graph bb in
  match List.find_opt (fun (_, edge) -> edge = cond) out with
  | Some (bb2, _) -> Some bb2
  | None -> None

let blocks_of { graph; _ } = Graph.vertices_of graph
let edges_of { graph; _ } = Graph.edges_of graph
let out_edges { graph; _ } block = Graph.out_neighbors graph block
