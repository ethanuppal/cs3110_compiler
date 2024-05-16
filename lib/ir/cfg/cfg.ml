module Graph = Digraph.Make (BasicBlock)

(** RI: [entry] is in [graph] and has no in neighbors. A block in [graph] must
    have zero out neighbors if its condition is [Never], one if its condition is
    [Always], and two if its condition is [Conditional]. *)
type t = {
  name : string list;
  entry : BasicBlock.t;
  graph : bool Graph.t;
}

let rep_ok cfg =
  if Rep_ok.check then (
    let vertices = Graph.vertices_of cfg.graph in
    if
      List.find_opt
        (fun bb -> BasicBlock.id_of bb = BasicBlock.id_of cfg.entry)
        vertices
      = None
    then failwith "rep_ok";
    List.iter
      (fun bb ->
        let out_degree = Graph.out_neighbors cfg.graph bb |> List.length in
        match BasicBlock.condition_of bb with
        | Never -> if out_degree <> 0 then failwith "rep_ok"
        | Always -> if out_degree <> 1 then failwith "rep_ok"
        | Conditional _ -> if out_degree <> 2 then failwith "rep_ok")
      vertices);
  cfg

let make name =
  let graph = Graph.empty () in
  let entry = BasicBlock.make () in
  Graph.add_vertex graph entry;
  { name; entry; graph } |> rep_ok

let name_of { name; _ } = name
let entry_to { entry; _ } = entry

let create_block cfg =
  let cfg = rep_ok cfg in
  let block = BasicBlock.make () in
  Graph.add_vertex cfg.graph block;
  block

let insert_branch cfg block cond bt bf =
  let cfg = rep_ok cfg in
  assert (BasicBlock.condition_of block = Never);
  Graph.add_edge cfg.graph block true bt;
  Graph.add_edge cfg.graph block false bf;
  BasicBlock.set_condition block cond

let insert_unconditional cfg pred succ =
  let cfg = rep_ok cfg in
  assert (BasicBlock.condition_of pred = Never);
  Graph.add_edge cfg.graph pred true succ;
  BasicBlock.set_condition pred Branch_condition.Always

let take_branch cfg bb cond =
  let cfg = rep_ok cfg in
  let out = Graph.out_neighbors cfg.graph bb in
  match List.find_opt (fun (_, edge) -> edge = cond) out with
  | Some (bb2, _) -> Some bb2
  | None -> None

let blocks_of { graph; _ } = Graph.vertices_of graph
let edges_of { graph; _ } = Graph.edges_of graph
let out_edges { graph; _ } block = Graph.out_neighbors graph block
let in_edges { graph; _ } block = Graph.in_neighbors graph block

let iter f cfg =
  let cfg = rep_ok cfg in
  Graph.dfs cfg.graph f cfg.entry;
  ignore (rep_ok cfg)

let exit_points cfg =
  let vertices = Graph.vertices_of cfg.graph in
  List.filter (fun v -> Graph.out_neighbors cfg.graph v = []) vertices

let to_string cfg =
  "func "
  ^ (cfg.name |> String.concat "::")
  ^ ":\n"
  ^ (blocks_of cfg
    |> List.map (fun bb ->
           let bb_string = BasicBlock.to_string bb in
           let dest_strings =
             out_edges cfg bb
             |> List.map (fun (dest, cond) ->
                    "\n    "
                    ^ (if cond then "true" else "false")
                    ^ " -> " ^ BasicBlock.label_for dest)
           in
           bb_string ^ String.concat "" dest_strings)
    |> String.concat "\n")
