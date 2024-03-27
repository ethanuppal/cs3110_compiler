open Util
(* Future: https://v2.ocaml.org/manual/gadts-tutorial.html *)
(* This is good for enforcing things at the type level. *)

(** An arithmetic operation. *)
type op =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod

(** An expression can be evaluated to a value. *)
type expr =
  | Var of string
  | Const of int
  | Infix of {
      lhs : expr;
      op : op;
      rhs : expr;
    }
  | Prefix of {
      op : op;
      rhs : expr;
    }
  | FunctionExpr of { body : stmt list }

(** A statement can be executed. *)
and stmt =
  | Call of string
    (* tbd better function support ia ExpressionStatement need to add in stuff
       baout returns and stuf lol*)
  | Declaration of string * expr
  | Assignment of string * expr
  | Function of {
      name : string;
      body : stmt list;
    }
  | Print of expr

(** A program is a series of statements. *)
type prog = stmt list

let pp_of string_of fmt x = Format.fprintf fmt "%s" (string_of x)

(** [string_of_op op] is the string representation of [op]. *)
let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"

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
  | Prefix { op; rhs } ->
      "(" ^ String.concat " " [ string_of_op op; string_of_expr rhs ] ^ ")"
  | FunctionExpr _ -> "<func>"

let expr_pp = pp_of string_of_expr

(** [string_of_stmt stmt] is the string representation of [stmt]. *)
let rec string_of_stmt ?(indent = 0) stmt =
  let indent_str = String.make indent ' ' in
  match stmt with
  | Call name -> indent_str ^ Printf.sprintf "%s()" name
  | Declaration (name, expr) ->
      indent_str ^ String.concat " " [ "let"; name; "="; string_of_expr expr ]
  | Assignment (name, expr) -> indent_str ^ name ^ " = " ^ string_of_expr expr
  | Function { name; body } ->
      Printf.sprintf "%sfunc %s() {\n" indent_str name
      ^ (body
        |> List.map (string_of_stmt ~indent:(indent + 2))
        |> String.concat "\n")
      ^ indent_str ^ "}"
  | Print e -> "print " ^ string_of_expr e

let stmt_pp = pp_of string_of_stmt

(** [string_of_prog prog] is the string representation of [prog]. *)
let string_of_prog = List.map string_of_stmt >> String.concat "\n"
