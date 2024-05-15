open AstType

let type_of_expr = function
  | Var { name = _; ty } -> ty
  | ConstInt _ -> Some Type.int_prim_type
  | ConstBool _ -> Some Type.bool_prim_type
  | Infix { lhs = _; op = _; rhs = _; ty } -> ty
  | Prefix { op = _; rhs = _; ty } -> ty
  | Call { name = _; args = _; ty } -> ty

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
      (name |> String.concat "::")
      ^ "("
      ^ (args |> List.map expr_to_string |> String.concat ", ")
      ^ ")"

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
      | Namespace { name; contents } ->
          "namespace " ^ name ^ " {\n"
          ^ (contents
            |> List.map (stmt_to_string_aux (indent ^ add_indent))
            |> String.concat "")
          ^ indent ^ "}"
    in
    indent ^ make_string stmt ^ "\n"
  in
  stmt_to_string_aux ""

let pp_op = Util.pp_of op_to_string
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
