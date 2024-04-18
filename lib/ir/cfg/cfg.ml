module Graph = Digraph.Make (Basic_block)

type t = bool Graph.t
