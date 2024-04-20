(** [t] is a basic block. *)
type t

(** [make ()] is a new basic block with a random [id] and a condition of [Never] *)
val make : unit -> t

(** [id_of basic_block] is the id of [basic_block]. *)
val id_of : t -> Id.id

(** [condition_of basic_block] is the condition associated with branching away
    from this basic block. *)
val condition_of : t -> Branch_condition.t

(** [set_condition bb cond] sets the condition of [bb] to [cond]. *)
val set_condition : t -> Branch_condition.t -> unit

(** [add_ir basic_block ir] adds [ir] to the end of [basic_block]. *)
val add_ir : t -> Ir.t -> unit

(** [to_list basic_block] are the IR operations in [basic_block] in order as a
    list. *)
val to_list : t -> Ir.t list

(** [equal bb1 bb2] is whether bb1 and bb2 have the same id. *)
val equal : t -> t -> bool

(** [hash bb] is a hash representing this basic block. [hash bb1 = hash bb2] iff
    [bb1 = bb2]. *)
val hash : t -> int
