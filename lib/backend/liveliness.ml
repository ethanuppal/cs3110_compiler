(* module VariableSet = Set.Make (Variable)

   type liveliness_data = { live_in : VariableSet.t; live_out : VariableSet.t;
   gen : VariableSet.t; kill : VariableSet.t; }

   module LivelinessData = Hashtbl.Make (Id) *)

let analyze = failwith "need good in neightbor function"
