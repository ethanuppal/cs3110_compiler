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

(** [branch cfg block cond] is a pair [(b1, b2)] *)
val branch : t -> Block.t -> Branch_condition.t -> Block.t * Block.t
