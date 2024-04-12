(** [t] is a basic block.. *)
type t

(** [make ()] is a new basic block. *)
val make : unit -> t

(** [label_of basic_block] is the label of [basic_block]. *)
val label_of : t -> Label.t

(** [add basic_block ir] adds [ir] to the end of [basic_block]. *)
val add : t -> Instr.t -> unit

(** [to_list basic_block] are the IR operations in [basic_block] in order as a
    list. *)
val to_list : t -> Instr.t list
