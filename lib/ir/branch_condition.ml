type t =
  | Always
  | Never
  | Conditional of Operand.t

(* TODO: pretty print *)
let to_string = function
  | Always -> "always"
  | Never -> "never"
  | Conditional op -> "if " ^ Operand.to_string op
