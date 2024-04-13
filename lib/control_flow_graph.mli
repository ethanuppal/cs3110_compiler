(** The type [t] represents a control flow graph. *)
type t

(** [make ()] is an empty control flow graph. *)
val make : unit -> t

(** [entry cfg] is the starting basic block for [cfg]. *)
val entry : t -> Basic_block.t

(** [make_bb_label cfg] is a label suitable for usage in [cfg]. Namely, no other
    label, barring those created with [Label.make_name], is permitted to be used
    in IR operations added through [insert_ir]. *)
val make_bb_label : t -> Label.t

(** [insert_ir cfg bb ir] inserts [ir] into [bb]. It is [None] if [ir] does not
    cause a branch and [Some bb2] if it does, and, in particular, branches to a
    basic block with label [label] such that [Basic_block.label_of bb2 = label].
    (You **must** supply the [label] by [make_bb_label cfg].) In that case,
    [bb2] will be added to [cfg] (or retrieved if a basic block labeled [label]
    has already been created).

    This function is the only valid way to insert IR into basic blocks managed
    by [cfg]. Namely, no other basic block modifier function should be used on
    such basic blocks. All accessor functions are permitted.

    Requires: [bb] is in [cfg]. *)
val insert_ir : t -> Basic_block.t -> Ir.t -> Basic_block.t option

(** [to_list cfg] are the basic blocks in [cfg]. *)
val to_list : t -> Basic_block.t list
