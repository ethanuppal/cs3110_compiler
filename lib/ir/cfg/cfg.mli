type t

(* TODO: ability to get all blocks for debugging and stuff, gonna need it later
   anyways. *)

(** [make ()] is a CFG with one empty block for the entry point. *)
val make : unit -> t

(** [entry cfg] is the block that is the entry point of [cfg]. *)
val entry : t -> Basic_block.t

(* TODO: can any of this be simplified by taking advantage of the type system?
   Especially the requirement on [cond]. *)

(** [branch cfg block cond] is a pair [(bt, bf)] where [bt] and [bf] are new
    blocks, [bt] being the jump target from [block] if [cond] is true, [bf]
    being the jump target from [block] if [cond] is false.

    [block] must already be in the graph.

    [cond] must be [Conditional], not [Never] or [Always]. [block] must not
    already be followed by another block. *)
val branch :
  t -> Basic_block.t -> Branch_condition.t -> Basic_block.t * Basic_block.t

(** [unconditionally cfg pred succ] makes [succ] unconditionally follow [pred].

    [pred] and [succ] must already be in the graph.

    [pred] must not already be followed by another block. *)
val unconditionally : t -> Basic_block.t -> Basic_block.t -> unit

val take_branch : t -> Basic_block.t -> bool -> Basic_block.t option

(* TODO: documentation *)

val blocks : t -> Basic_block.t list
val edges : t -> (Basic_block.t * bool * Basic_block.t) list

(* TODO: pretty print *)
