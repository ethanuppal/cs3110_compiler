open Util

(** A value of type [VariableSet.t] is a set of IR variables. *)
module VariableSet : sig
  include Set.S with type elt = Variable.t

  val to_string : t -> string
end

(** Liveliness analysis of a basic block. *)
module BasicBlockAnalysis : sig
  (** Values of type [t] are liveliness analyses for a given basic block. *)
  type t

  (** [make bb] is an empty liveliness analysis for the basic block [bb]. It is
      guaranteed to never mutate or copy [bb] internally.

      Requires: [Basic_block.length_of bb > 0]. *)
  val make : Basic_block.t -> t

  (** [live_in analysis] is the set of variables live at the start of the the
      analyzed basic block.

      Time Complexity: [O(1)]. *)
  val live_in : t -> VariableSet.t

  (** [live_out analysis] is the set of variables live at the end of the the
      analyzed basic block.

      Time Complexity: [O(1)]. *)
  val live_out : t -> VariableSet.t

  (** [live_before_instr analysis index] is the set of variables live before the
      [index]th instruction in the basic block for which [analysis] was created.

      Time Complexity: [O(1)]. *)
  val live_before_instr : t -> int -> VariableSet.t

  (** [live_after_instr analysis index] is the set of variables live after the
      [index]th instruction in the basic block for which [analysis] was created.

      Time Complexity: [O(1)]. *)
  val live_after_instr : t -> int -> VariableSet.t

  (** [to_string analysis] is a string representation of [analysis]. *)
  val to_string : t -> string
end

(** [analysis_of cfg] is an association between the basic blocks in [cfg] and
    their liveliness analyses. In particular, let [a] be the result of this
    function and let [bb] be a basic block in [cfg] Then,
    [Util.IdMap.find a (Basic_block.id_of bb)] is the liveliness analysis of
    [bb].

    Requires: every basic block in [cfg] has at least one IR instruction. *)
val analysis_of : Cfg.t -> BasicBlockAnalysis.t IdMap.t
