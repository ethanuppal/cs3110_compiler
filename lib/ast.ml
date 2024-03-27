open Util
(* Future: https://v2.ocaml.org/manual/gadts-tutorial.html *)
(* This is good for enforcing things at the type level. *)

(** An arithmetic operation. *)
type op =
  | Plus
  | Minus
  | Times

(** An expression can be evaluated to a value. *)
type expr =
  | Var of string
  | Const of int
  | Infix of {
      lhs : expr;
      op : op;
      rhs : expr;
    }

(** A statement can be executed. *)
type stmt =
  | Declaration of string * expr
  | Print of expr

(** A program is a series of statements. *)
type prog = stmt list

let pp_of string_of fmt x = Format.fprintf fmt "%s" (string_of x)

(** [string_of_op op] is the string representation of [op]. *)
let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"

let op_pp = pp_of string_of_op

(** [string_of_expr expr] is the string representation of [expr]. *)
let rec string_of_expr = function
  | Var name -> name
  | Const i -> string_of_int i
  | Infix { lhs; op; rhs } ->
      "("
      ^ String.concat " "
          [ string_of_expr lhs; string_of_op op; string_of_expr rhs ]
      ^ ")"

let expr_pp = pp_of string_of_expr

(** [string_of_stmt stmt] is the string representation of [stmt]. *)
let string_of_stmt = function
  | Declaration (vname, e) ->
      String.concat " " [ "let"; vname; "="; string_of_expr e ]
  | Print e -> "print " ^ string_of_expr e

let stmt_pp = pp_of string_of_stmt

(** [string_of_prog prog] is the string representation of [prog]. *)
let string_of_prog = List.map string_of_stmt >> String.concat "\n"
