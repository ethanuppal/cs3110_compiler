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
  | BitAnd

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
  | Call of {
      name : string;
      args : expr list;
      mutable ty : Type.t option;
    }

(** A statement can be executed. *)
and stmt =
  | If of {
      cond : expr;
      body : stmt list;
    }
  | ExprStatement of expr
  | Declaration of {
      name : string;
      hint : Type.t option;
      expr : expr;
    }
  | Assignment of string * expr
  | Function of {
      name : string;
      params : (string * Type.t) list;
      return : Type.t;
      body : stmt list;
    }
  | Print of expr
  | Return of expr option

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
  | Call { name = _; args = _; ty } -> ty

(** [expr_is_const expr] if and only if [expr] is a constant (i.e., cannot have
    an address taken of it). *)
let expr_is_const = function
  | ConstInt _ | ConstBool _ -> true
  | _ -> false

let op_to_string op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | Equals -> "=="
  | BitAnd -> "&"

let rec expr_to_string = function
  | Var { name; ty = _ } -> name
  | ConstInt n -> string_of_int n
  | ConstBool b -> string_of_bool b
  | Infix { lhs; op; rhs; ty = _ } ->
      "(" ^ expr_to_string lhs ^ " " ^ op_to_string op ^ " "
      ^ expr_to_string rhs ^ ")"
  | Prefix { op; rhs; ty = _ } ->
      "(" ^ op_to_string op ^ expr_to_string rhs ^ ")"
  | Call { name; args; ty = _ } ->
      name ^ "(" ^ (args |> List.map expr_to_string |> String.concat ", ") ^ ")"

let stmt_to_string =
  let add_indent = String.make 4 ' ' in
  let rec stmt_to_string_aux indent stmt =
    let make_string = function
      | ExprStatement expr -> expr_to_string expr
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
      | Function { name; params; return; body } ->
          "func " ^ name ^ "("
          ^ (params
            |> List.map (fun (name, ty) -> name ^ ": " ^ Type.to_string ty)
            |> String.concat ", ")
          ^ ") -> " ^ Type.to_string return ^ " {\n"
          ^ (body
            |> List.map (stmt_to_string_aux (indent ^ add_indent))
            |> String.concat "")
          ^ indent ^ "}"
      | Print expr -> "print " ^ expr_to_string expr
      | If { cond; body } ->
          "if " ^ expr_to_string cond ^ " {\n"
          ^ (body
            |> List.map (stmt_to_string_aux (indent ^ add_indent))
            |> String.concat "")
          ^ indent ^ "}"
      | Return expr_opt -> (
          match expr_opt with
          | None -> "return"
          | Some expr -> "return " ^ expr_to_string expr)
    in
    indent ^ make_string stmt ^ "\n"
  in
  stmt_to_string_aux ""

(** TODO: to string functions *)
let pp_op fmt =
  let open Util in
  op_to_string >> Format.pp_print_string fmt

let pp_expr = Util.pp_of expr_to_string
let pp_stmt = Util.pp_of stmt_to_string

let pp_prog fmt prog =
  Format.pp_open_vbox fmt 0;
  List.iter
    (fun stmt ->
      pp_stmt fmt stmt;
      Format.pp_print_cut fmt ())
    prog;
  Format.pp_close_box fmt ()
