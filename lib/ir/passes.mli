(** Constant folding optimization pass. *)
module ConstFold : Pass.Sig

(** Copy propagation optimization pass. *)
module CopyProp : Pass.Sig

(** Dead code elimination optimization pass. *)
module DeadCode : Pass.Sig

(** [apply passes cfg liveliness] applies each pass in [passes] to [cfg] in
    order, using the liveliness information for [cfg] ([liveliness]).

    Requires that [liveliness] was generated from the current state of [cfg]. *)
val apply :
  Pass.t list -> Cfg.t -> Liveliness.BasicBlockAnalysis.t IdMap.t -> unit
