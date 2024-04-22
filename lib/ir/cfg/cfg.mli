type t

(* TODO: ability to get all blocks for debugging and stuff, gonna need it later
   anyways. *)

(** [make ()] is a CFG with one empty block for the entry point. *)
val make : unit -> t

(** [entry cfg] is the block that is the entry point of [cfg]. *)
val entry : t -> Basic_block.t

(** [create_block cfg] is a new block added to [cfg] with no inputs and no
    outputs. *)
val create_block : t -> Basic_block.t

(* TODO: can any of this be simplified by taking advantage of the type system?
   Especially the requirement on [cond]. *)

(** [insert_branch cfg block cond bt bf] creates a branch from [block] that goes
    to [bt] when [cond] is true and goes to [bf] when [cond] is false.

    [block], [bt], and [bf] must already be in the graph.

    [cond] must be [Conditional], not [Never] or [Always]. [block] must not
    already be followed by another block. *)
val insert_branch :
  t ->
  Basic_block.t ->
  Branch_condition.t ->
  Basic_block.t ->
  Basic_block.t ->
  unit

(** [insert_unconditional cfg pred succ] makes [succ] unconditionally follow
    [pred].

    [pred] and [succ] must already be in the graph.

    [pred] must not already be followed by another block. *)
val insert_unconditional : t -> Basic_block.t -> Basic_block.t -> unit

val take_branch : t -> Basic_block.t -> bool -> Basic_block.t option

(** [blocks_of cfg] is a list of all blocks in [cfg]. *)
val blocks_of : t -> Basic_block.t list

(** [edges_of cfg] is a list of all edges [(v1, b, v2)] in [cfg] where [v1] is
    the start block, [v2] is the destination block, and [b] indicates wheter
    [v2] follows [v1] when the condition of [v1] is true or false. *)
val edges_of : t -> (Basic_block.t * bool * Basic_block.t) list

(** [edges_from cfg block] is a list of edges from [block]. *)
val out_edges : t -> Basic_block.t -> (Basic_block.t * bool) list

(* TODO: pretty print *)
