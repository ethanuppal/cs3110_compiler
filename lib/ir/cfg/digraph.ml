(* TODO: disallow edge replacement *)
(* TODO: rep invariant? *)

module Make (V : Hashtbl.HashedType) = struct
  module T = Hashtbl.Make (V)

  (** AF: If [out_graph] is a hash table of hash tables as such,
      [{ 
        v1 = { vi = ea; vj = eb; ... };
        v2 = { vk = ec; vl = ed; ... };
        ...
        vn = { ... }
      }]
      then this graph is a directed graph with [n] vertices and
      [len vi forall i, 1 <= i <= n] edges. Each mapping [vj = ea] in the entry
      corresponding to [vi] represents an edge from [vi] to [vj] labeled with
      [ea].

      RI: if the mapping corresponding to node [vi] in [out_graph] contains a
      mapping [vj = e], then [vi = e] must be in the mapping corresponding to
      node [vj] in [in_graph]. In other words, [in_graph] must be the reverse
      mapping of [out_graph]. *)

  type 'edge t = {
    out_graph : 'edge T.t T.t;
    in_graph : 'edge T.t T.t;
  }

  let check_rep { out_graph; in_graph } =
    let check_correspondence v1 v2 edge graph =
      let neighbors = T.find graph v1 in
      assert (T.find neighbors v2 = edge)
    in

    T.iter
      (fun v_from out_neighbors ->
        T.iter
          (fun v_to edge -> check_correspondence v_to v_from edge in_graph)
          out_neighbors)
      out_graph;
    T.iter
      (fun v_to in_neighbors ->
        T.iter
          (fun v_from edge -> check_correspondence v_from v_to edge out_graph)
          in_neighbors)
      in_graph

  let rep_ok graph =
    if Rep_ok.check then check_rep graph;
    graph

  let empty () = { out_graph = T.create 4; in_graph = T.create 4 }

  let add_vertex graph vertex =
    let graph = rep_ok graph in
    assert (not (T.mem graph.out_graph vertex));

    T.add graph.out_graph vertex (T.create 2);
    T.add graph.in_graph vertex (T.create 2)

  let add_edge graph v1 e v2 =
    let graph = rep_ok graph in
    assert (T.mem graph.out_graph v1);
    assert (T.mem graph.out_graph v2);

    let v1_out = T.find graph.out_graph v1 in
    T.replace v1_out v2 e;
    let v2_in = T.find graph.in_graph v2 in
    T.replace v2_in v1 e

  let in_neighbors graph vertex =
    let graph = rep_ok graph in
    T.find graph.in_graph vertex |> T.to_seq |> List.of_seq

  let out_neighbors graph vertex =
    let graph = rep_ok graph in
    T.find graph.out_graph vertex |> T.to_seq |> List.of_seq

  let vertices_of graph =
    let graph = rep_ok graph in
    graph.out_graph |> T.to_seq_keys |> List.of_seq

  let edges_of graph =
    let graph = rep_ok graph in
    T.to_seq graph.out_graph
    |> Seq.flat_map (fun (v1, out) ->
           T.to_seq out |> Seq.map (fun (v2, e) -> (v1, e, v2)))
    |> List.of_seq

  let dfs graph f start =
    let graph = rep_ok graph in
    let visited = T.create 16 in
    let rec dfs_aux vertex =
      f vertex;
      T.add visited vertex true;
      out_neighbors graph vertex |> List.map fst
      |> List.filter (T.mem visited)
      |> List.iter dfs_aux
    in
    dfs_aux start
end
