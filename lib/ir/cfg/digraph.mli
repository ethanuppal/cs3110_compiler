module Make (V : Hashtbl.HashedType) : sig
  type 'edge t

  (** [empty ()] is a fresh empty digraph. *)
  val empty : unit -> 'edge t

  (** [add_vertex graph vertex] adds the given [vertex] to [graph] with no
      neighbors. Requires that [vertex] is not already in [graph]. *)
  val add_vertex : 'edge t -> V.t -> unit

  (** [add_edge graph v1 e v2] adds an edge from [v1] to [v2] labeled with [e].
      Requires that [v1] and [v2] are already in [graph]. If there already
      exists a directed edge from [v1] to [v2], its label is replaced with [e]. *)
  val add_edge : 'edge t -> V.t -> 'edge -> V.t -> unit

  (** [in_neighbors graph vertex] is a list of vertices that have outgoing edges
      to [vertex] coupled with their edge labels. Requires that [vertex] is in
      [graph]. *)
  val in_neighbors : 'edge t -> V.t -> (V.t * 'edge) list

  (** [out_neighbors graph vertex] is a list of vertices that [vertex] has
      outgoing edges to coupled with their edge labels. Requires that [vertex]
      is in [graph]. *)
  val out_neighbors : 'edge t -> V.t -> (V.t * 'edge) list

  (* TODO: documentation and tests *)

  val vertices : 'edge t -> V.t list
  val edges : 'edge t -> (V.t * 'edge * V.t) list
end
