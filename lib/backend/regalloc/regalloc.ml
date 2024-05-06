module VarMap = Map.Make (Variable)

type var_reg_map = Asm.Register.t VarMap.t

let allocate_for cfg =
  ignore cfg;
  failwith "not implemented"
