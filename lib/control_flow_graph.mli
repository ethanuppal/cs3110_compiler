(** The type [t] represents a control flow graph. *)
type t

(** [make ()] is an empty control flow graph. *)
val make : unit -> t

(** [entry cfg] is the starting basic block for [cfg]. *)
val entry : t -> Basic_block.t

(** [insert_ir cfg bb ir] inserts [ir] into [bb]. It is [None] if [ir] does not
    cause a branch and [Some bb2] if it does, and, in particular, branches to a
    basic block with label [label] such that [Basic_block.label_of bb2 = label].
    In that case, [bb2] will be added to [cfg] (or retrieved if a basic block
    labeled [label] has already been created).

    Requires: [bb] is in [cfg]. *)
val insert_ir : t -> Basic_block.t -> Ir.t -> Basic_block.t option

(** [to_list cfg] are the basic blocks in [cfg]. *)
val to_list : t -> Basic_block.t list
