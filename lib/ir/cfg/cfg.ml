module Node = struct
  type t = Basic_block.t * Branch_condition.t

  let equal (bb1, _) (bb2, _) = Basic_block.equal bb1 bb2
  let hash (bb, _) = Basic_block.hash bb
end

module Graph = Digraph.Make (Node)

type t = bool Graph.t

let make () = Graph.empty ()
