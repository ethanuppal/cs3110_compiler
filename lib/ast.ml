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

let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Assign -> "="
  | Let -> "="

let rec string_of_expr e =
  match e with
  | Var name -> name
  | Const i -> string_of_int i
  | Infix {lhs = lhs; op = op; rhs = rhs} -> String.concat " " [string_of_expr lhs; string_of_op op; string_of_expr rhs]

let string_of_stmt s =
  match s with 
  | Declaration (vname, e) -> String.concat " " ["let"; vname; "="; string_of_expr e]
  | Print e -> "print " ^ (string_of_expr e)

let string_of_prog p = 
  List.map string_of_expr p |> String.concat "\n"