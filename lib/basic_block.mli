(** [t] is a basic block.. *)
type t

(** [make gen] is a new basic block. Let this result be [bb]; then, it is
    guaranteed that [label_of bb |> Label.id_of] is generated by a call to
    [Id.Gen.next gen]. *)
val make : Id.Gen.t -> t

(** [label_of basic_block] is the label of [basic_block]. *)
val label_of : t -> Label.t

(** [add basic_block ir] adds [ir] to the end of [basic_block]. *)
val add : t -> Ir.t -> unit

(** [to_list basic_block] are the IR operations in [basic_block] in order as a
    list. *)
val to_list : t -> Ir.t list
