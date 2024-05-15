(** Represents the hardware allocated for a variable. [Register reg] means the
    variable has been allocated to a register. [Spill i] means the variable is
    to be spilled to the stack. The location [i] will be unique and count up
    from zero for an allocation scheme produced by [allocate_for]. *)
type allocation =
  | Register of Asm.Register.t
  | Spill of int

(** [allocate_for cfg registers liveliness ordering] allocates a register in
    [registers] (or a spill location) to each variable in [cfg] based on the
    liveliness analysis of [cfg] ([liveliness]) and an arbitrary ordering of the
    instructions in [cfg] ([ordering]).

    Requires that [liveliness] and [ordering] were both produced from [cfg] in
    the state that it is being passed in. *)
val allocate_for :
  Cfg.t ->
  Asm.Register.t list ->
  Liveliness.BasicBlockAnalysis.t IdMap.t ->
  InstrOrdering.t ->
  allocation VariableMap.t
