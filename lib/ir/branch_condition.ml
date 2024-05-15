type t =
  | Always
  | Never
  | Conditional of Operand.t

let to_string = function
  | Always -> "true"
  | Conditional (Constant cond) when cond <> 0 -> "true"
  | Never | Conditional (Constant 0) -> "false"
  | Conditional op -> "if " ^ Operand.to_string op
