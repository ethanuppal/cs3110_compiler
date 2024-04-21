(* TODO: disallow edge replacement *)
(* TODO: rep invariant? *)

module Make (V : Hashtbl.HashedType) = struct
  module T = Hashtbl.Make (V)

  (* A list is fine or even better here since we have a max out degree count of
     two. *)

  (** AF:
      [{ 
        v1 = [ (vi, ea); (vj, eb); ... ];
        v2 = [ (vk, ec); (vl, ed); ... ];
        ...
        vn = [ ... ]
      }]
      is a directed graph with [n] vertices and [len vi forall i, 1 <= i <= n]
      edges. Each element in [vi] [(vj, ea)] represents an outgoing edge from
      [vi] to [vj] with edge label [ea]. *)
  type 'edge t = (V.t * 'edge) list T.t

  let empty () = T.create 4

  let add_vertex graph vertex =
    assert (not (T.mem graph vertex));
    T.add graph vertex []

  let add_edge graph v1 e v2 =
    assert (T.mem graph v1);
    assert (T.mem graph v2);

    let v1_lst = T.find graph v1 in
    let removed = List.filter (fun (v, _) -> not (V.equal v v2)) v1_lst in
    T.replace graph v1 ((v2, e) :: removed)

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
           filter_in_edges from_vertex vertex edges |> List.to_seq)
    |> List.of_seq

  let out_neighbors graph vertex =
    assert (T.mem graph vertex);
    T.find graph vertex

  let vertices graph = T.to_seq_keys graph |> List.of_seq

  (* TODO: edges *)
  let edges _graph = []
end
