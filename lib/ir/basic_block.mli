(** [t] is a basic block. *)
type t

(** [make ()] is a new basic block with a random [id] and a condition of [Never] *)
val make : unit -> t

(** [id_of bb] is the id of [bb]. *)
val id_of : t -> Id.id

(** [length_of bb] is the number of instructions in [bb]. *)
val length_of : t -> int

(** [condition_of bb] is the condition associated with branching away from this
    basic block. *)
val condition_of : t -> Branch_condition.t

(** [set_condition bb cond] sets the condition of [bb] to [cond]. *)
val set_condition : t -> Branch_condition.t -> unit

(** [add_ir bb ir] adds [ir] to the end of [bb]. *)
val add_ir : t -> Ir.t -> unit

(** [get_ir bb idx] is the IR instruction at index [idx] in [bb].

    Requires: [Basic_block.length_of bb > idx]. *)
val get_ir : t -> int -> Ir.t

(** [get_orig_idx bb idx] is the original index of the IR instruction at index
    [idx] in [bb]; this original index will never changed.

    Requires: [Basic_block.length_of bb > idx]. *)
val get_orig_idx : t -> int -> int

(** [set_ir bb idx ir] replaces the IR instruction at index [idx] in [bb] with
    [ir].

    Requires: [Basic_block.length_of bb > idx]. *)
val set_ir : t -> int -> Ir.t -> unit

(** [rem_ir bb idx] removes the IR instruction at index [idx] in [bb], shifting
    all the subsequent indices/IR instructions backward.

    Requires: [Basic_block.length_of bb > idx]. *)
val rem_ir : t -> int -> unit

(** [to_list bb] are the IR operations in [bb] in order as a list. *)
val to_list : t -> Ir.t list

(** [label_for bb] is the assembler label for [bb]. *)
val label_for : t -> string

(** [equal bb1 bb2] is whether bb1 and bb2 have the same id. *)
val equal : t -> t -> bool

(** [hash bb] is a hash representing this basic block. [hash bb1 = hash bb2] iff
    [bb1 = bb2]. *)
val hash : t -> int

(** [to_string bb] returns a string representation of the basic block [bb].
    The string representation includes the label of the basic block, followed
    by the string representation of each instruction in the basic block.
    If the basic block has a branch condition, it is also included in the
    string representation. *)
val to_string : t -> string
