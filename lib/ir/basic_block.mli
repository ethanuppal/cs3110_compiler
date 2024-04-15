(** [t] is a basic block. *)
type t

(** [make condition] is a new basic block with [condition] as the final branch
    condition. It will have a randomly generated id. *)
val make : Branch_condition.t -> t

(** [id_of basic_block] is the id of [basic_block]. *)
val id_of : t -> Id.id

(** [add basic_block ir] adds [ir] to the end of [basic_block]. *)
val add : t -> Ir.t -> unit

(** [to_list basic_block] are the IR operations in [basic_block] in order as a
    list. *)
val to_list : t -> Ir.t list
