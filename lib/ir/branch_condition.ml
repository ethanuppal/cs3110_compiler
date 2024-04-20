type unconditional
type conditional
type cond_operands = Operand.t * Operand.t

(* GADT:
   https://stackoverflow.com/questions/24653301/accepting-only-one-variant-of-sum-type-as-ocaml-function-parameter *)
(* allows us to only accept a subset of variants *)
type _ t =
  | Always : unconditional t
  | Never : unconditional t
  | Equal : cond_operands -> conditional t
  | Unequal : cond_operands -> conditional t
  | LessThan : cond_operands -> conditional t
  | GreaterThan : cond_operands -> conditional t
  | AtMost : cond_operands -> conditional t
  | AtLeast : cond_operands -> conditional t

let to_string (type a) : a t -> string =
  let open Printf in
  function
  | Always -> "true"
  | Never -> "false"
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
