module Graph = Digraph.Make (Basic_block)

(* TODO: rn the take of this module is "here are basic blocks. you can do
   whatever you want with them. don't mess it up." this could be improved but I
   don't know how. *)

(* TODO: rep_ok *)
(* TODO: enforce some of this with type system somehow? *)

(** RI: [entry] is in [graph] and has no in neighbors. A block in [graph] must
    have zero out neighbors if its condition is [Never], one if its condition is
    [Always], and two if its condition is [Conditional]. *)
type t = {
  entry : Basic_block.t;
  graph : bool Graph.t;
}

let make () =
  let graph = Graph.empty () in
  let entry = Basic_block.make () in
  Graph.add_vertex graph entry;
  { entry; graph }

let entry { entry; _ } = entry

let create_block { graph; _ } =
  let block = Basic_block.make () in
  Graph.add_vertex graph block;
  block

let insert_branch { graph; _ } block cond bt bf =
  assert (Basic_block.condition_of block = Never);
  Graph.add_edge graph block true bt;
  Graph.add_edge graph block false bf;
  Basic_block.set_condition block cond

let insert_unconditional { graph; _ } pred succ =
  assert (Basic_block.condition_of pred = Never);
  Graph.add_edge graph pred true succ;
  Basic_block.set_condition pred Branch_condition.Always

let take_branch { graph; _ } bb cond =
  let out = Graph.out_neighbors graph bb in
  match List.find_opt (fun (_, edge) -> edge = cond) out with
  | Some (bb2, _) -> Some bb2
  | None -> None

let blocks_of { graph; _ } = Graph.vertices graph
let edges_of { graph; _ } = Graph.edges graph
let out_edges { graph; _ } block = Graph.out_neighbors graph block
