open X86ISTMB
open Alcotest

type edge = int

module Vertex = Int
module Graph = Digraph.Make (Vertex)

let vertex : Vertex.t QCheck.arbitrary = QCheck.int

let single_vertex () =
  let v = 10 in
  let graph = Graph.empty () in
  Graph.add_vertex graph v;
  (check int) "no in neighbors" 0 (Graph.in_neighbors graph v |> List.length);
  (check int) "no out neighbors" 0 (Graph.out_neighbors graph v |> List.length)

let test_suite =
  ("lib/cfg/digraph.ml", [ test_case "single vertex" `Quick single_vertex ])
