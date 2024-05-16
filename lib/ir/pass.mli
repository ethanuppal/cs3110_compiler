(** Represents an optimization pass that can be executed on IR. *)
type t

(** [make f] is a basic pass that runs a basic block and its liveliness analysis
    through [f]. *)
val make : (BasicBlock.t * Liveliness.BasicBlockAnalysis.t -> unit) -> t

(** [sequence p1 p2] is a pass that first runs [p1] then [p2]. *)
val sequence : t -> t -> t

(** [combine lst] is a pass that runs each pass in [list] in sequence. *)
val combine : t list -> t

(** [repeat n pass] is a pass that runs [pass] [n] times.*)
val repeat : int -> t -> t

(** [execute pass block liveliness] runs [block] and its analysis [liveliness]
    through [pass]. [block] may be mutated. [liveliness] must correspond to the
    analysis of [block] in its current state. *)
val execute : t -> BasicBlock.t -> Liveliness.BasicBlockAnalysis.t -> unit

(** A signature for modules that implement optimization passes. *)
module type Sig = sig
  (** The pass itself, usually a basic pass created with [make]. *)
  val pass : t
end
