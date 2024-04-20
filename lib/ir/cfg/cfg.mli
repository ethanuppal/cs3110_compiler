type t

(** A block in this control flow graph that can be used to store IR. *)
module Block : sig
  type t

  val add_ir : t -> Ir.t -> unit
end

(** [make ()] is a CFG with one empty block for the entry point. *)
val make : unit -> t

(** [entry cfg] is the block that is the entry point of [cfg]. *)
val entry : t -> Block.t

(* TODO: can any of this be simplified by taking advantage of the type system?
   Especially the requirement on [cond]. *)

(** [branch cfg block cond] is a pair [(bt, bf)] where [bt] and [bf] are new
    blocks, [bt] being the jump target from [block] if [cond] is true, [bf]
    being the jump target from [block] if [cond] is false.

    [cond] must be [Conditional], not [Never] or [Always]. [block] must not
    already be followed by another block. *)
val branch : t -> Block.t -> Branch_condition.t -> Block.t * Block.t
