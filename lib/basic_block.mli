(** [id] is unique identifier of the basic block. *)
type id

(** [t] is a basic block.. *)
type t

(** [make ()] is a new basic block. *)
val make : unit -> t

(** [id_of basic_block] is the identifier of [basic_block]. *)
val id_of : t -> id

(** [label_of basic_block] is the label of [basic_block] suitable for emission
    in an object file as a symbol. *)
val label_of : t -> string

(** [add basic_block ir] adds [ir] to the end of [basic_block]. *)
val add : t -> Ir.t -> unit

(** [to_list basic_block] are the IR operations in [basic_block] in order as a
    list. *)
val to_list : t -> Ir.t list
