(* Future: https://v2.ocaml.org/manual/gadts-tutorial.html *)
(* This is good for enforcing things at the type level. *)

(** An arithmetic operation. *)
type op =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Equals

(** An expression can be evaluated to a value. *)
type expr =
  | Var of {
      name : string;
      mutable ty : Type.t option;
    }
  | ConstInt of int
  | ConstBool of bool
  | Infix of {
      lhs : expr;
      op : op;
      rhs : expr;
      mutable ty : Type.t option;
    }
  | Prefix of {
      op : op;
      rhs : expr;
      mutable ty : Type.t option;
    }
  (* ignore, only for interpreter *)
  | FunctionExpr of { body : stmt list }

(** A statement can be executed. *)
and stmt =
  | Call of string
    (* tbd better function support ia ExpressionStatement need to add in stuff
       baout returns and stuf lol*)
  | Declaration of {
      name : string;
      hint : Type.t option;
      expr : expr;
    }
  | Assignment of string * expr
  | Function of {
      name : string;
      body : stmt list;
    }
  | Print of expr

(** A program is a series of statements. *)
type prog = stmt list

(** [type_of_expr expr] is the type of [expr] to the extent that it is currently
    resolved. *)
let type_of_expr = function
  | Var { name = _; ty } -> ty
  | ConstInt _ -> Some Type.int_prim_type
  | ConstBool _ -> Some Type.bool_prim_type
  | Infix { lhs = _; op = _; rhs = _; ty } -> ty
  | Prefix { op = _; rhs = _; ty } -> ty
  | FunctionExpr _ -> None

let op_to_string op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | Equals -> "=="

let rec expr_to_string = function
  | Var { name; ty = _ } -> name
  | ConstInt n -> string_of_int n
  | ConstBool b -> string_of_bool b
  | Infix { lhs; op; rhs; ty = _ } ->
      "(" ^ expr_to_string lhs ^ " " ^ op_to_string op ^ " "
      ^ expr_to_string rhs ^ ")"
  | Prefix { op; rhs; ty = _ } ->
      "(" ^ op_to_string op ^ expr_to_string rhs ^ ")"
  | FunctionExpr _ -> "<func>"

let stmt_to_string =
  let add_indent = String.make 4 ' ' in
  let rec stmt_to_string_aux indent stmt =
    let make_string = function
      | Call name -> name ^ "()"
      | Declaration { name; hint; expr } ->
          let expr_type = type_of_expr expr in
          let display_type = if expr_type = None then hint else expr_type in
          let hint_str =
            match display_type with
            | Some t -> ": " ^ Type.to_string t
            | None -> ""
          in
          "let " ^ name ^ hint_str ^ " = " ^ expr_to_string expr
      | Assignment (name, expr) -> name ^ " = " ^ expr_to_string expr
      | Function { name; body } ->
          "func " ^ name ^ "() {\n"
          ^ (body
            |> List.map (stmt_to_string_aux (indent ^ add_indent))
            |> String.concat "")
          ^ "}"
      | Print expr -> "print " ^ expr_to_string expr
    in
    indent ^ make_string stmt ^ "\n"
  in
  stmt_to_string_aux ""

(** TODO: to string functions *)
let pp_op fmt =
  let open Util in
  op_to_string >> Format.pp_print_string fmt

let rec pp_expr fmt = function
  | Var { name; _ } -> Format.pp_print_string fmt name
  | ConstInt i -> Format.pp_print_int fmt i
  | ConstBool b -> Format.pp_print_bool fmt b
  | Infix { lhs; op; rhs; _ } ->
      Format.pp_print_string fmt "(";
      pp_expr fmt lhs;
      Format.pp_print_string fmt " ";
      pp_op fmt op;
      Format.pp_print_string fmt " ";
      pp_expr fmt rhs;
      Format.pp_print_string fmt ")"
  | Prefix { op; rhs; _ } ->
      Format.pp_print_string fmt "(";
      pp_op fmt op;
      pp_expr fmt rhs;
      Format.pp_print_string fmt ")"
  | FunctionExpr _ -> Format.pp_print_string fmt "<func>"

let rec pp_stmt fmt = function
  | Call name -> Format.fprintf fmt "%s()" name
  | Declaration { name; hint; expr } ->
      Format.fprintf fmt "let %s%s = " name
        (let expr_type = type_of_expr expr in
         let display_type = if expr_type = None then hint else expr_type in
         match display_type with
         | Some ty -> ": " ^ Type.to_string ty
         | None -> "");
      pp_expr fmt expr
  | Assignment (name, expr) ->
      Format.fprintf fmt "%s = " name;
      pp_expr fmt expr
  | Function { name; body } ->
      Format.fprintf fmt "func %s() {" name;
      (* Go down a line and indent by two *)
      Format.pp_print_break fmt 0 2;
      Format.pp_force_newline fmt ();
      Format.pp_open_hvbox fmt 0;
      Format.pp_print_list pp_stmt fmt body;
      Format.pp_close_box fmt ();
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "}"
  | Print e ->
      Format.pp_print_string fmt "print ";
      pp_expr fmt e

let pp_prog fmt prog =
  Format.pp_open_vbox fmt 0;
  List.iter
    (fun stmt ->
      pp_stmt fmt stmt;
      Format.pp_print_cut fmt ())
    prog;
  Format.pp_close_box fmt ()
