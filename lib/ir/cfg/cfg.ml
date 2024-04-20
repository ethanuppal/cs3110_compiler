module Block = Basic_block
module Graph = Digraph.Make (Block)

(* TODO: enforce some of this with type system somehow? *)
(* TODO: rep_ok *)

(** RI: [entry] is in [graph] and has no in neighbors. A block in [graph] must
    have zero out neighbors if its condition is [Never], one if its condition is
    [Always], and two if its condition is [Conditional]. *)
type t = {
  entry : Block.t;
  graph : bool Graph.t;
}

let make () =
  let graph = Graph.empty () in
  let entry = Block.make () in
  Graph.add_vertex graph entry;
  { entry; graph }

let entry { entry; _ } = entry

let branch { graph; _ } block cond =
  assert (Basic_block.condition_of block = Never);
  let bt = Block.make () in
  let bf = Block.make () in
  Graph.add_vertex graph bt;
  Graph.add_vertex graph bf;
  Graph.add_edge graph block true bt;
  Graph.add_edge graph block false bf;
  Basic_block.set_condition block cond;
  (bt, bf)
