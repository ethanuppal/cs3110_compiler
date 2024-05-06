(** [t] is a control flow graph.

    Every basic block yielded through this API must only be modified with
    through this API. In particular, all accessor functions in the [Basic_block]
    module are permitted. Once the control flow graph generation is complete for
    a set of basic blocks, then it is permitted to modify the contents (but not
    the condition) of the basic blocks. *)
type t

(** [make name] is a control-flow graph for a function named [name] with one
    empty block for the entry point. *)
val make : string -> t

(** [name_of cfg] is the name of the function represented by [cfg]. *)
val name_of : t -> string

(** [entry_to cfg] is the block that is the entry point of [cfg].

    Review the specification for [t] on the basic blocks yielded through this
    API. *)
val entry_to : t -> Basic_block.t

(** [create_block cfg] is a new block added to [cfg] with no inputs and no
    outputs.

    Review the specification for [t] on the basic blocks yielded through this
    API. *)
val create_block : t -> Basic_block.t

(* TODO: can any of this be simplified by taking advantage of the type system?
   Especially the requirement on [cond]. *)

(** [insert_branch cfg block cond bt bf] creates a branch from [block] that goes
    to [bt] when [cond] is true and goes to [bf] when [cond] is false.

    Review the specification for [t] on the basic blocks yielded through this
    API.

    Requires: [block], [bt], and [bf] must already be in the graph.
    Additiionally, [cond] must be [Conditional], not [Never] or [Always].
    [block] must not already be followed by another block. *)
val insert_branch :
  t ->
  Basic_block.t ->
  Branch_condition.t ->
  Basic_block.t ->
  Basic_block.t ->
  unit

(** [insert_unconditional cfg pred succ] makes [succ] unconditionally follow
    [pred].

    Review the specification for [t] on the basic blocks yielded through this
    API.

    Requires: [pred] and [succ] must already be in the graph, and [pred] must
    not already be followed by another block. *)
val insert_unconditional : t -> Basic_block.t -> Basic_block.t -> unit

(** [take_branch cfg bb cond] is [Some bb2], where [bb2] is basic block
    branching from [bb] in [cfg] with branch condition [cond], or [None] if no
    such branch exists.

    Review the specification for [t] on the basic blocks yielded through this
    API. *)
val take_branch : t -> Basic_block.t -> bool -> Basic_block.t option

(** [blocks_of cfg] is a list of all blocks in [cfg]. *)
val blocks_of : t -> Basic_block.t list

(** [edges_of cfg] is a list of all edges [(v1, b, v2)] in [cfg] where [v1] is
    the start block, [v2] is the destination block, and [b] indicates wheter
    [v2] follows [v1] when the condition of [v1] is true or false. *)
val edges_of : t -> (Basic_block.t * bool * Basic_block.t) list

(** [out_edges cfg block] is a list of edges from [block]. *)
val out_edges : t -> Basic_block.t -> (Basic_block.t * bool) list

(** [in_edges cfg block] is a list of edges into [block]. *)
val in_edges : t -> Basic_block.t -> (Basic_block.t * bool) list

(** [iter f cfg] calls [f] on every basic block in [cfg]. *)
val iter : (Basic_block.t -> unit) -> t -> unit

(** [exit_points cfg] is a list of all blocks that exit the program in [cfg]. *)
val exit_points : t -> Basic_block.t list

(* TODO: pretty print *)

val to_string : t -> string
