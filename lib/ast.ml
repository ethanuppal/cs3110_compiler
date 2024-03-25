(* Future: https://v2.ocaml.org/manual/gadts-tutorial.html *) 
(* This is good for enforcing things at the type level. *)

type op =
  | Plus
  | Minus
  | Times
  | Print
  | Assign
  | Let

type expr = 
  | Var of string
  | Const of int
  | Declaration of string * expr
  | Infix of {lhs: expr; op: op; rhs: expr}
  | Unary of {op: op; rhs: expr}