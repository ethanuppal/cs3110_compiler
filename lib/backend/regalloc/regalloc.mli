open Util
module VarTbl : Hashtbl.S with type key = Variable.t

type allocation =
  | Register of Asm.Register.t
  | Spill

val allocate_for :
  Cfg.t ->
  Liveliness.BasicBlockAnalysis.t IdMap.t ->
  InstrOrdering.t ->
  allocation VarTbl.t
