module VarMap : Map.S with type key = Variable.t

type var_reg_map = Asm.Register.t VarMap.t

val allocate_for : Cfg.t -> var_reg_map
