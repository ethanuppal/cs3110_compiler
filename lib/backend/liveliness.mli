open Util

(** A value of type [VariableSet.t] is a set of IR variables. *)
module VariableSet : sig
  include Set.S with type elt = Variable.t
end

(** Liveliness analysis of a basic block. *)
module Analysis : sig
  (** Values of type [t] are liveliness analyses for a given basic block. *)
  type t

  (** [make ~gen:gen ~kill:kill] is an empty liveliness analysis for a basic
      block with the set of variables read before assignment [gen] and the set
      of variables assigned to [kill]. *)
  val make : gen:VariableSet.t -> kill:VariableSet.t -> t

  (** [live_in analysis] is the set of variables live at the start of the the
      analyzed basic block. *)
  val live_in : t -> VariableSet.t

  (** [live_out analysis] is the set of variables live at the end of the the
      analyzed basic block. *)
  val live_out : t -> VariableSet.t

  val to_string : t -> string
end

(** [analysis_of bb_list] is an association between the basic blocks in
    [bb_list] and their liveliness analyses. In particular, let [a] be the
    result of this function and let [bb] be a basic block such that
    [List.mem bb bb_list]. Then, [Util.IdMap.find a (Basic_block.id_of bb)] is
    the liveliness analysis of [bb]. *)
val analysis_of : Basic_block.t list -> Analysis.t IdMap.t
