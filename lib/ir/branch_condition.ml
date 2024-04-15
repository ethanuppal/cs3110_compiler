type t =
  | Unconditional
  | Equal of Operand.t * Operand.t
  | Unequal of Operand.t * Operand.t
  | LessThan of Operand.t * Operand.t
  | GreaterThan of Operand.t * Operand.t
  | AtMost of Operand.t * Operand.t
  | AtLeast of Operand.t * Operand.t

let to_string =
  let open Printf in
  function
  | Unconditional -> "true"
  | Equal (o1, o2) ->
      sprintf "%s == %s" (Operand.to_string o1) (Operand.to_string o2)
  | Unequal (o1, o2) ->
      sprintf "%s != %s" (Operand.to_string o1) (Operand.to_string o2)
  | LessThan (o1, o2) ->
      sprintf "%s < %s" (Operand.to_string o1) (Operand.to_string o2)
  | GreaterThan (o1, o2) ->
      sprintf "%s > %s" (Operand.to_string o1) (Operand.to_string o2)
  | AtMost (o1, o2) ->
      sprintf "%s <= %s" (Operand.to_string o1) (Operand.to_string o2)
  | AtLeast (o1, o2) ->
      sprintf "%s >= %s" (Operand.to_string o1) (Operand.to_string o2)
