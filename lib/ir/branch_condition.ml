type op =
  | Equal
  | Unequal
  | LessThan
  | GreaterThan
  | AtMost
  | AtLeast

type t =
  | Always
  | Never
  | Conditional of op * Operand.t * Operand.t

let to_string =
  let open Printf in
  function
  | Always -> "true"
  | Never -> "false"
  | Conditional (op, o1, o2) -> (
      match op with
      | Equal ->
          sprintf "%s == %s" (Operand.to_string o1) (Operand.to_string o2)
      | Unequal ->
          sprintf "%s != %s" (Operand.to_string o1) (Operand.to_string o2)
      | LessThan ->
          sprintf "%s < %s" (Operand.to_string o1) (Operand.to_string o2)
      | GreaterThan ->
          sprintf "%s > %s" (Operand.to_string o1) (Operand.to_string o2)
      | AtMost ->
          sprintf "%s <= %s" (Operand.to_string o1) (Operand.to_string o2)
      | AtLeast ->
          sprintf "%s >= %s" (Operand.to_string o1) (Operand.to_string o2))
