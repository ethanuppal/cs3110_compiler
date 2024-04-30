type constant = int

type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Sub of Variable.t * Operand.t * Operand.t
  | Ref of Variable.t * Operand.t
  | Deref of Variable.t * Operand.t
  | TestEqual of Variable.t * Operand.t * Operand.t
  | DebugPrint of Operand.t
(* | Call of string * Operand.t list *)

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
  | Ref (r, o) ->
      sprintf "%s = &%s" (Variable.to_string r) (Operand.to_string o)
  | Deref (r, o) ->
      sprintf "%s = *%s" (Variable.to_string r) (Operand.to_string o)
  | TestEqual (r, o1, o2) ->
      sprintf "%s = %s == %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | DebugPrint op -> sprintf "debug_print %s" (Operand.to_string op)
