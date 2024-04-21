type constant = int

(* TODO: how to support more ops? keep consistent with ast? *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Sub of Variable.t * Operand.t * Operand.t
  | Equal of Variable.t * Operand.t * Operand.t
  | DebugPrint of Operand.t

let to_string =
  let open Printf in
  function
  | Assign (r, o) ->
      sprintf "%s = %s" (Variable.to_string r) (Operand.to_string o)
  | Add (r, o1, o2) ->
      sprintf "%s = %s + %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Sub (r, o1, o2) ->
      sprintf "%s = %s - %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Equal (r, o1, o2) ->
      sprintf "%s = %s == %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | DebugPrint op -> sprintf "debug_print %s" (Operand.to_string op)
