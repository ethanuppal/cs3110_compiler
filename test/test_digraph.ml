open X86ISTMB
module Vertex = Char
module Graph = Digraph.Make (Vertex)

let single_vertex =
  let test () =
    let open Alcotest in
    let v = 'a' in
    let graph = Graph.empty () in
    Graph.add_vertex graph v;
    (check int) "no in neighbors" 0 (Graph.in_neighbors graph v |> List.length);
    (check int) "no out neighbors" 0 (Graph.out_neighbors graph v |> List.length)
  in
  Alcotest.test_case "single vertex" `Quick test

let check_neighbors = Alcotest.(check (list (pair char int)))

let full_graph =
  let test () =
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
  in
  Alcotest.test_case "full graph" `Quick test

let replace_edge =
  let test () =
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
  in
  Alcotest.test_case "replace edge" `Quick test

let gen_graph density =
  let open QCheck2.Gen in
  let* n = small_nat in
  let vertices = Seq.ints 0 |> Seq.take n |> Seq.map Char.chr |> Array.of_seq in
  let gen_edge =
    frequency [ (density, int >|= Option.some); (100 - density, pure None) ]
  in
  let gen_adj = array_size (pure n) (array_size (pure n) gen_edge) in

  gen_adj >|= fun adjacency ->
  let graph = Graph.empty () in
  Array.iter (Graph.add_vertex graph) vertices;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match adjacency.(i).(j) with
      | Some edge -> Graph.add_edge graph vertices.(i) edge vertices.(j)
      | None -> ()
    done
  done;
  graph

let print_graph graph =
  let verts = Graph.vertices_of graph in
  let vert_strings = List.map (Printf.sprintf "%c") verts in
  let edges = Graph.edges_of graph in
  let edge_strings =
    List.map (fun (v1, e, v2) -> Printf.sprintf "%c =%i=> %c" v1 e v2) edges
  in
  let s =
    Printf.sprintf "Vertices: %s; Edges: %s"
      (String.concat ", " vert_strings)
      (String.concat " " edge_strings)
  in
  QCheck2.Print.string s

let in_out_neighbors density =
  let open QCheck2 in
  let name = Printf.sprintf "in out symmetric (density=%i)" density in
  let test =
    Test.make ~name ~count:100 ~print:print_graph (gen_graph density)
      (fun graph ->
        let vertices = Graph.vertices_of graph in
        let results =
          vertices
          |> List.map (fun v ->
                 let out_neighbors = Graph.out_neighbors graph v in
                 List.map
                   (fun (neighbor, edge) ->
                     Graph.in_neighbors graph neighbor |> List.mem (v, edge))
                   out_neighbors)
          |> List.flatten
        in
        List.for_all Fun.id results)
  in
  QCheck_alcotest.to_alcotest ~long:true test

let test_suite =
  ( "lib/ir/cfg/digraph.ml",
    [ single_vertex; full_graph; replace_edge; in_out_neighbors 50 ] )
