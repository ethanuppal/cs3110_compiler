module Make (V : Hashtbl.HashedType) = struct
  module T = Hashtbl.Make (V)

  (** AF:
      [{ 
        v1 = [| (vi, ea); (vj, eb); ... |];
        v2 = [| (vk, ec); (vl, ed); ... |];
        ...
        vn = [| ... |]
      }]
      is a directed graph with [n] vertices and [len vi forall i, 1 <= i <= n]
      edges. Each element in [vi] [(vj, ea)] represents an outgoing edge from
      [vi] to [vj] with edge label [ea]. *)
  type 'edge t = (V.t * 'edge) BatDynArray.t T.t

  let empty () = T.create 4

  let add_vertex graph vertex =
    assert (not (T.mem graph vertex));
    T.add graph vertex (BatDynArray.make 2)

  let add_edge graph v1 e v2 =
    assert (T.mem graph v1);
    assert (T.mem graph v2);

    let v1_arr = T.find graph v1 in
    try
      let i = BatDynArray.findi (fun (v, _) -> V.equal v v2) v1_arr in
      BatDynArray.set v1_arr i (v2, e)
    with Not_found -> BatDynArray.add v1_arr (v2, e)

  let in_neighbors graph vertex =
    assert (T.mem graph vertex);

    let filter_in_edges from_vertex to_vertex out_edges =
      out_edges
      |> List.filter (fun (dest_vertex, _label) ->
             V.equal dest_vertex to_vertex)
      |> List.map (fun (_dest_vertex, label) -> (from_vertex, label))
    in

    T.to_seq graph
    |> Seq.flat_map (fun (from_vertex, edges) ->
           let out_edge_list = BatDynArray.to_list edges in
           filter_in_edges from_vertex vertex out_edge_list |> List.to_seq)
    |> List.of_seq

  let out_neighbors graph vertex =
    assert (T.mem graph vertex);
    BatDynArray.to_list (T.find graph vertex)
end
