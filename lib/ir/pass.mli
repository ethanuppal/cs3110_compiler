type t

val make : (Basic_block.t * Liveliness.BasicBlockAnalysis.t -> unit) -> t
val sequence : t -> t -> t
val combine : t list -> t
val repeat : int -> t -> t
val execute : t -> Basic_block.t -> Liveliness.BasicBlockAnalysis.t -> unit

module type PASS = sig
  val pass : t
end
