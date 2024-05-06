type t = unit
type instr_id = Id.t * int

let make cfg =
  ignore cfg;
  failwith "not implemented"

let compare ordering id1 id2 =
  ignore ordering;
  ignore id1;
  ignore id2;
  failwith "not implemented"
