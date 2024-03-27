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

(** TODO: to string functions *)
let pp_op fmt op =
  let char =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Mod -> "%"
  in
  Format.pp_print_string fmt char

let rec pp_expr fmt = function
  | Var name -> Format.pp_print_string fmt name
  | Const i -> Format.pp_print_int fmt i
  | Infix { lhs; op; rhs } ->
      Format.pp_print_string fmt "(";
      pp_expr fmt lhs;
      pp_op fmt op;
      pp_expr fmt rhs;
      Format.pp_print_string fmt ")"
  | Prefix { op; rhs } ->
      Format.pp_print_string fmt "(";
      pp_op fmt op;
      pp_expr fmt rhs;
      Format.pp_print_string fmt ")"
  | FunctionExpr _ -> Format.pp_print_string fmt "<func>"

let rec pp_stmt fmt = function
  | Call name -> Format.fprintf fmt "%s()" name
  | Declaration (name, expr) ->
      Format.fprintf fmt "let %s = " name;
      pp_expr fmt expr
  | Assignment (name, expr) ->
      Format.fprintf fmt "%s = " name;
      pp_expr fmt expr
  | Function { name; body } ->
      Format.fprintf fmt "func %s () {" name;
      (* Go down a line and indent by two *)
      Format.pp_print_break fmt 0 2;
      Format.pp_open_vbox fmt 0;
      Format.pp_print_list pp_stmt fmt body;
      Format.pp_close_box fmt ();
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "}"
  | Print e ->
      Format.pp_print_string fmt "print ";
      pp_expr fmt e

let prog_pp fmt prog =
  Format.pp_open_vbox fmt 0;
  List.iter
    (fun stmt ->
      pp_stmt fmt stmt;
      Format.pp_print_cut fmt ())
    prog;
  Format.pp_close_box fmt ()
