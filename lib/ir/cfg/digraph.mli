(** [vertex] uniquely identifies a vertex. *)
type vertex_id

(** ['a t] is a directed graph with vertices of type ['a]. *)
type 'a t

(** [make ()] is an empty directed graph. *)
val make : unit -> 'a t

(** [vertex_count dg] is the number of vertices in [dg]. *)
val vertex_count : 'a t -> int

(** [vertex_id_of_int (n - 1)] is the handle for the vertex added to an
    arbitrary directed graph [dg : t] after [n] calls to [add_vertex] on [dg].
    The vertex may not exist yet, so any attempt to access the result before
    [vertex_count dg >= n] is strictly invalid. *)
val vertex_id_of_int : int -> vertex_id

(** If the vertex to which [v] is a handle was added to an arbitrary directed
    graph [dg : t] after [n] calls to [add_vertex] on [dg], then
    [int_of_vertex_id v] is [n - 1]. *)
val int_of_vertex_id : vertex_id -> int

(** [get dg v] is the vertex in the graph [dg] with handle [v].

    Requires: the vertex is in [dg]. *)
val get : 'a t -> vertex_id -> 'a

(** [vertices_of dg] are the vertices of [dg] as a sequence. *)
val vertices_of : 'a t -> vertex_id Seq.t

(** [add_vertex dg value] is a handle to [value] after inserting it into the
    graph [dg] *)
val add_vertex : 'a t -> 'a -> vertex_id

(** [add_edge dg v1 v2] adds an edge from the the vertex associated with [v1]
    and the vertex associated with [v2]. *)
val add_edge : 'a t -> vertex_id -> vertex_id -> unit
