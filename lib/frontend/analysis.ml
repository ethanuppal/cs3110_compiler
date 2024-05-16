open AstType
open Ast

type analysis_error_info =
  | GeneralInfo
  | NameInfo of { symbol : string }
  | TypeInfo of
      [ `Mismatch of Type.t * Type.t
      | `InvalidSig of string * Type.t list
      | `DerefRValue of Type.t
      ]
  | HaltInfo of { name : string }

exception
  AnalyzerError of {
    info : analysis_error_info;
    msg : string option;
    ast : (expr, stmt) Either.t;
  }

let general_error ?(msg = "") ast =
  AnalyzerError
    { info = GeneralInfo; msg = (if msg = "" then None else Some msg); ast }

let name_error symbol ?(msg = "") ast =
  AnalyzerError
    {
      info = NameInfo { symbol };
      msg = (if msg = "" then None else Some msg);
      ast;
    }

let type_mismatch_error exp_ty act_ty ?(msg = "") ast =
  AnalyzerError
    {
      info = TypeInfo (`Mismatch (exp_ty, act_ty));
      msg = (if msg = "" then None else Some msg);
      ast;
    }

let type_sig_error name invalid_tys ?(msg = "") ast =
  AnalyzerError
    {
      info = TypeInfo (`InvalidSig (name, invalid_tys));
      msg = (if msg = "" then None else Some msg);
      ast;
    }

let deref_rval_error ty ?(msg = "") ast =
  AnalyzerError
    {
      info = TypeInfo (`DerefRValue ty);
      msg = (if msg = "" then None else Some msg);
      ast;
    }

let halt_error name ?(msg = "") ast =
  AnalyzerError
    {
      info = HaltInfo { name };
      msg = (if msg = "" then None else Some msg);
      ast;
    }

(** [bind_var_to_type ctx name ty ast] binds [name] to have [ty] in [ctx] as
    called in analyzing node [ast].

    @raise AnalyzerError if [name] has already been bound in the top scope. *)
let bind_name_to_type ctx name ty ast =
  match Context.get_local ctx name with
  | None -> Context.insert ctx name ty
  | Some _ -> raise (name_error name ~msg:"invalid redeclaration" ast)

(** [get_var_type ctx name ast] is the type of [name] in [ctx] as called in
    analyzing node [ast].

    @raise AnalyzerError if [name] is not bound in [ctx]. *)
let get_type_of_name ctx name ast =
  match Context.get ctx name with
  | Some ty -> ty
  | None -> raise (name_error name ~msg:"unbound variable" ast)

let analyzer_error_to_string info msg _ =
  let msg_str =
    match msg with
    | None -> ""
    | Some msg -> ": " ^ msg
  in
  match info with
  | GeneralInfo -> "Analyzer error" ^ msg_str
  | NameInfo { symbol } -> "Name error" ^ msg_str ^ ": '" ^ symbol ^ "'"
  | TypeInfo issue ->
      let start_str = "Type error" ^ msg_str in
      let rest_str =
        match issue with
        | `Mismatch (exp_ty, act_ty) ->
            Printf.sprintf "expected %s but received %s" (Type.to_string exp_ty)
              (Type.to_string act_ty)
        | `InvalidSig (token, invalid_tys) ->
            Printf.sprintf
              "no overload for '%s' exists with parameter types (%s)" token
              (invalid_tys |> List.map Type.to_string |> String.concat ", ")
        | `DerefRValue ty ->
            Printf.sprintf "cannot dereference r-value of type %s"
              (Type.to_string ty)
      in
      start_str ^ ": " ^ rest_str
  | HaltInfo { name } ->
      "Halting error: function '" ^ name ^ "' does not return from all paths"
      ^ msg_str

let () =
  Printexc.register_printer (function
    | AnalyzerError { info; msg; ast } ->
        Some (analyzer_error_to_string info msg ast)
    | _ -> None)

(** [infer_expr ctx expr] is the type [expr] will be assigned a type based on
    [ctx].

    Side Effect: [expr.ty] will be updated to reflect the resulting type.

    @raise AnalyzerError on failure. *)
let rec infer_expr (ctx : Type.t Context.t) expr =
  let infer_expr_aux expr =
    match expr with
    | Var var -> var.ty <- Some (get_type_of_name ctx var.name (Left expr))
    | ConstInt _ -> ()
    | ConstBool _ -> ()
    | StringLiteral _ -> ()
    | Infix infix -> (
        let lhs_ty = infer_expr ctx infix.lhs in
        let rhs_ty = infer_expr ctx infix.rhs in
        let raise_error () =
          raise
            (type_sig_error (op_to_string infix.op) [ lhs_ty; rhs_ty ]
               (Left expr))
        in
        match infix.op with
        | Plus | Minus | Times | Divide | Mod ->
            if lhs_ty = Type.int_prim_type && rhs_ty = Type.int_prim_type then
              infix.ty <- Some Type.int_prim_type
            else raise_error ()
        | Equals ->
            if lhs_ty = rhs_ty then infix.ty <- Some Type.bool_prim_type
            else raise_error ()
        | _ -> raise_error ())
    | Prefix prefix -> (
        let rhs_ty = infer_expr ctx prefix.rhs in
        let raise_error () =
          raise (type_sig_error (op_to_string prefix.op) [ rhs_ty ] (Left expr))
        in
        match prefix.op with
        | Plus | Minus ->
            if rhs_ty = Type.int_prim_type then
              prefix.ty <- Some Type.int_prim_type
            else raise_error ()
        | Times -> (
            match Type.deref rhs_ty with
            | Some ty -> prefix.ty <- Some ty
            | None -> raise_error ())
        | BitAnd ->
            if expr_is_const prefix.rhs then
              raise (deref_rval_error rhs_ty (Left expr));
            prefix.ty <- Some (Type.Pointer rhs_ty)
        | _ -> raise_error ())
    | Call call ->
        let name = call.name |> String.concat "::" in
        let arg_tys = List.map (infer_expr ctx) call.args in
        let exp_ty = get_type_of_name ctx name (Left expr) in
        let exp_params, exp_return =
          match exp_ty with
          | FunctionType { params; return } -> (params, return)
          | _ ->
              raise
                (name_error name ~msg:"only functions can be called" (Left expr))
        in
        if not (List.for_all2 Type.equal exp_params arg_tys) then (
          let open Util in
          print_endline "exp";
          List.iter (Type.to_string >> prerr_endline) exp_params;
          print_endline "act";
          List.iter (Type.to_string >> prerr_endline) arg_tys;
          raise (type_sig_error name arg_tys (Left expr)));
        call.ty <- Some exp_return
  in
  infer_expr_aux expr;
  Option.get (type_of_expr expr)

(* TODO: add Terminal and Nonterminal checks *)

(** [infer_stmt ctx return_ctx stmt] is the type [stmt] will be assigned a type
    based on [ctx] when the environment intends a return type of [return_ctx].

    @raise AnalyzerError on failure. *)
let rec infer_stmt (ctx : Type.t Context.t) return_ctx (stmt : stmt) :
    Type.stmt_type =
  match stmt with
  | Declaration { name; hint; expr } ->
      let expr_ty = infer_expr ctx expr in
      (match hint with
      | None -> ()
      | Some hint_ty ->
          if hint_ty <> expr_ty then
            raise
              (type_mismatch_error hint_ty expr_ty ~msg:"in let statement"
                 (Right stmt)));
      bind_name_to_type ctx name expr_ty (Right stmt);
      Nonterminal
  | Print expr ->
      infer_expr ctx expr |> ignore;
      Nonterminal
  | Function _ | Namespace _ | ForeignFunction _ | DeclaredFunction _ ->
      raise
        (general_error ~msg:"functions can only be written at top level"
           (Right stmt))
  | If { cond; body } ->
      let cond_ty = infer_expr ctx cond in
      if cond_ty <> Type.bool_prim_type then
        raise
          (type_mismatch_error Type.bool_prim_type cond_ty
             ~msg:"in if statement condition" (Right stmt));
      infer_body ctx return_ctx body
  | Assignment (name, expr) ->
      let exp_ty = get_type_of_name ctx name (Right stmt) in
      let expr_ty = infer_expr ctx expr in
      if exp_ty <> expr_ty then
        raise
          (type_mismatch_error exp_ty expr_ty
             ~msg:"variable types cannot be modified" (Right stmt));
      Nonterminal
  | ExprStatement expr ->
      ignore (infer_expr ctx expr);
      Nonterminal
  | Return expr_opt ->
      let expr_ty =
        match expr_opt with
        | None -> Type.unit_prim_type
        | Some expr -> infer_expr ctx expr
      in
      if return_ctx <> expr_ty then
        raise
          (type_mismatch_error return_ctx expr_ty ~msg:"invalid return type"
             (Right stmt));
      Terminal

(* TODO: final statement always needs to be Terminal *)
and infer_body ctx return_ctx stmts =
  Context.push ctx;
  let ty =
    List.fold_left
      (fun _ stmt -> infer_stmt ctx return_ctx stmt)
      Nonterminal stmts
  in
  Context.pop ctx;
  ty

let rec infer_top_level ctx stmt =
  match stmt with
  | Namespace { name; contents } ->
      Context.add_namespace ctx name;
      List.iter (infer_top_level ctx) contents;
      Context.pop_namespace ctx
  | ForeignFunction { name; params; return } ->
      let fun_ty = Type.FunctionType { params; return } in
      bind_name_to_type ctx ("ffi::" ^ name) fun_ty (Right stmt)
  | DeclaredFunction { name; params; return } ->
      let fun_ty = Type.FunctionType { params; return } in
      bind_name_to_type ctx
        (Context.in_namespace ctx name |> String.concat "::")
        fun_ty (Right stmt)
  | Function { name; params; return; body } ->
      let fun_ty = Type.FunctionType { params = List.map snd params; return } in
      bind_name_to_type ctx
        (Context.in_namespace ctx name |> String.concat "::")
        fun_ty (Right stmt);
      Context.push ctx;
      List.iter
        (fun (name, ty) -> bind_name_to_type ctx name ty (Right stmt))
        params;
      if infer_body ctx return body <> Terminal then
        raise (halt_error name (Right stmt));
      Context.pop ctx
  | _ ->
      raise
        (general_error ~msg:"only functions can be written at top level"
           (Right stmt))

let infer prog =
  let ctx : Type.t Context.t = Context.make () in
  Context.push ctx;
  List.iter (infer_top_level ctx) prog
