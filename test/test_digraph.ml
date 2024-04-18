open X86ISTMB
open Alcotest
module Vertex = Char
module Graph = Digraph.Make (Vertex)

(** TODO: this would be nice to test with qcheck. hard with mutability. come
    back to it later. *)

let single_vertex () =
  let v = 'a' in
  let graph = Graph.empty () in
  Graph.add_vertex graph v;
  (check int) "no in neighbors" 0 (Graph.in_neighbors graph v |> List.length);
  (check int) "no out neighbors" 0 (Graph.out_neighbors graph v |> List.length)

let full_graph () =
  let graph = Graph.empty () in
  List.iter (Graph.add_vertex graph) [ 'a'; 'b'; 'c'; 'd'; 'e' ];
  Graph.add_edge graph 'a' 1 'b';
  Graph.add_edge graph 'a' 2 'd';
  Graph.add_edge graph 'b' 3 'a';
  Graph.add_edge graph 'b' 4 'e';
  Graph.add_edge graph 'e' 5 'd';
  Graph.add_edge graph 'd' 6 'd';

  let in_a = Graph.in_neighbors graph 'a' |> List.sort compare in
  let in_b = Graph.in_neighbors graph 'b' |> List.sort compare in
  let in_c = Graph.in_neighbors graph 'c' |> List.sort compare in
  let in_d = Graph.in_neighbors graph 'd' |> List.sort compare in
  let in_e = Graph.in_neighbors graph 'e' |> List.sort compare in

  let check_neighbors = check (list (pair char int)) in

  check_neighbors "in neighbors of a" [ ('b', 3) ] in_a;
  check_neighbors "in neighbors of b" [ ('a', 1) ] in_b;
  check_neighbors "in neighbors of c" [] in_c;
  check_neighbors "in neighbors of d" [ ('a', 2); ('d', 6); ('e', 5) ] in_d;
  check_neighbors "in neighbors of e" [ ('b', 4) ] in_e;

  let out_a = Graph.out_neighbors graph 'a' |> List.sort compare in
  let out_b = Graph.out_neighbors graph 'b' |> List.sort compare in
  let out_c = Graph.out_neighbors graph 'c' |> List.sort compare in
  let out_d = Graph.out_neighbors graph 'd' |> List.sort compare in
  let out_e = Graph.out_neighbors graph 'e' |> List.sort compare in

  check_neighbors "out neighbors of a" [ ('b', 1); ('d', 2) ] out_a;
  check_neighbors "out neighbors of b" [ ('a', 3); ('e', 4) ] out_b;
  check_neighbors "out neighbors of c" [] out_c;
  check_neighbors "out neighbors of d" [ ('d', 6) ] out_d;
  check_neighbors "out neighbors of e" [ ('d', 5) ] out_e

let test_suite =
  ( "lib/cfg/digraph.ml",
    [
      test_case "single vertex" `Quick single_vertex;
      test_case "full graph" `Quick full_graph;
    ] )
