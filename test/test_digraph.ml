open X86ISTMB
open Alcotest
module Vertex = Char
module Graph = Digraph.Make (Vertex)

(** TODO: this would be nice to test with qcheck. directed graphs lend
    themselves very well to property based testing. *)

let single_vertex () =
  let v = 'a' in
  let graph = Graph.empty () in
  Graph.add_vertex graph v;
  (check int) "no in neighbors" 0 (Graph.in_neighbors graph v |> List.length);
  (check int) "no out neighbors" 0 (Graph.out_neighbors graph v |> List.length)

let check_neighbors = check (list (pair char int))

let full_graph () =
  let graph = Graph.empty () in
  List.iter (Graph.add_vertex graph) [ 'a'; 'b'; 'c'; 'd'; 'e' ];
  Graph.add_edge graph 'a' 1 'b';
  Graph.add_edge graph 'a' 2 'd';
  Graph.add_edge graph 'b' 3 'a';
  Graph.add_edge graph 'b' 4 'e';
  Graph.add_edge graph 'e' 5 'd';
  Graph.add_edge graph 'd' 6 'd';

  let sorted_in_neighbors char =
    Graph.in_neighbors graph char |> List.sort compare
  in

  let in_a = sorted_in_neighbors 'a' in
  let in_b = sorted_in_neighbors 'b' in
  let in_c = sorted_in_neighbors 'c' in
  let in_d = sorted_in_neighbors 'd' in
  let in_e = sorted_in_neighbors 'e' in

  check_neighbors "in neighbors of a" [ ('b', 3) ] in_a;
  check_neighbors "in neighbors of b" [ ('a', 1) ] in_b;
  check_neighbors "in neighbors of c" [] in_c;
  check_neighbors "in neighbors of d" [ ('a', 2); ('d', 6); ('e', 5) ] in_d;
  check_neighbors "in neighbors of e" [ ('b', 4) ] in_e;

  let sorted_out_neighbors char =
    Graph.out_neighbors graph char |> List.sort compare
  in

  let out_a = sorted_out_neighbors 'a' in
  let out_b = sorted_out_neighbors 'b' in
  let out_c = sorted_out_neighbors 'c' in
  let out_d = sorted_out_neighbors 'd' in
  let out_e = sorted_out_neighbors 'e' in

  check_neighbors "out neighbors of a" [ ('b', 1); ('d', 2) ] out_a;
  check_neighbors "out neighbors of b" [ ('a', 3); ('e', 4) ] out_b;
  check_neighbors "out neighbors of c" [] out_c;
  check_neighbors "out neighbors of d" [ ('d', 6) ] out_d;
  check_neighbors "out neighbors of e" [ ('d', 5) ] out_e

let replace_edge () =
  let graph = Graph.empty () in
  Graph.add_vertex graph 'a';
  Graph.add_vertex graph 'b';
  Graph.add_edge graph 'a' 1 'b';

  Graph.add_edge graph 'a' 2 'b';
  check_neighbors "out neighbors of a"
    [ ('b', 2) ]
    (Graph.out_neighbors graph 'a');

  check_neighbors "in neighbors of b"
    [ ('a', 2) ]
    (Graph.in_neighbors graph 'b')

let test_suite =
  ( "lib/cfg/digraph.ml",
    [
      test_case "single vertex" `Quick single_vertex;
      test_case "full graph" `Quick full_graph;
      test_case "replace edge" `Quick replace_edge;
    ] )
