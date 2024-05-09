type t
type instr_id = Id.t * int

(** [make cfg] is an ordering of the instructions in [cfg] in chronological
    order. *)
val make : Cfg.t -> t

(** [compare ordering id1 id2] is 0 if [id1 = id2], negative if the instruction
    with id [id1] comes before the instruction with id [id2], and positive if it
    comes after.

    Requires that [id1] and [id2] are valid ids for the cfg this ordering was
    constructed from. *)
val compare : t -> instr_id -> instr_id -> int
