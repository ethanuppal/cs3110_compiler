module type Vertex = sig
  type t

  (** [id_of v] is a unique id representing this vertex. [a] and [b] should be
      equal if and only if [id_of a = id_of b]*)
  val id_of : t -> Id.id
end

module Make (M : Vertex) : sig
  type 'edge t

  val add_node : 'edge t -> M.t -> unit
  val add_edge : 'edge t -> M.t -> 'edge -> M.t -> unit
end
