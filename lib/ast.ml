(* Future: https://v2.ocaml.org/manual/gadts-tutorial.html *) 
(* This is good for enforcing things at the type level. *)

type op =
  | Plus
  | Minus
  | Times
  | Assign
  | Let

type expr = 
  | Var of string
  | Const of int
  | Infix of {lhs: expr; op: op; rhs: expr}

type stmt =
  | Declaration of string * expr
  | Print of expr

type prog = stmt list