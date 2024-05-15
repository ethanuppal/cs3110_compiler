type allocation =
  | Register of Asm.Register.t
  | Spill of int

val registers : Asm.Register.t list

val allocate_for :
  Cfg.t ->
  Liveliness.BasicBlockAnalysis.t IdMap.t ->
  InstrOrdering.t ->
  allocation VariableMap.t
