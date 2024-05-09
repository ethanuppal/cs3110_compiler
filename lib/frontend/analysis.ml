open Ast
open Util

type analysis_error_info =
  | GeneralInfo
  | NameInfo of { symbol : string }
  | TypeInfo of
      [ `Mismatch of Type.t * Type.t
      | `InvalidSig of string * Type.t list
      | `DerefRValue of Type.t
      ]

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
let get_var_type ctx name ast =
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
    | Var var -> var.ty <- Some (get_var_type ctx var.name (Left expr))
    | ConstInt _ -> ()
    | ConstBool _ -> ()
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
  in
  infer_expr_aux expr;
  Option.get (type_of_expr expr)

(* TODO: add Terminal and Nonterminal checks *)

(** [infer_stmt ctx stmt] is the type [stmt] will be assigned a type based on
    [ctx].

    @raise AnalyzerError on failure. *)
let rec infer_stmt (ctx : Type.t Context.t) stmt =
  (match stmt with
  | Declaration { name; hint; expr } ->
      let expr_ty = infer_expr ctx expr in
      (match hint with
      | None -> ()
      | Some hint_ty ->
          if hint_ty <> expr_ty then
            raise
              (type_mismatch_error hint_ty expr_ty ~msg:"in let statement"
                 (Right stmt)));
      bind_name_to_type ctx name expr_ty (Right stmt)
  | Print expr -> infer_expr ctx expr |> ignore
  | Function _ ->
      raise
        (general_error ~msg:"functions can only be written at top level"
           (Right stmt))
  | If { cond; body } ->
      let cond_ty = infer_expr ctx cond in
      if cond_ty <> Type.bool_prim_type then
        raise
          (type_mismatch_error Type.bool_prim_type cond_ty
             ~msg:"in if statement condition" (Right stmt));
      infer_body ctx body
  | Assignment (name, expr) ->
      let exp_ty = get_var_type ctx name (Right stmt) in
      let expr_ty = infer_expr ctx expr in
      if exp_ty <> expr_ty then
        raise
          (type_mismatch_error exp_ty expr_ty
             ~msg:"variable types cannot be modified" (Right stmt))
  | Call _ -> failwith "not impl");
  Type.Nonterminal

(* TODO: final statement always needs to be Terminal *)
and infer_body ctx stmts =
  Context.push ctx;
  List.iter (infer_stmt ctx >> ignore) stmts;
  Context.pop ctx

let infer prog =
  let ctx : Type.t Context.t = Context.make () in
  Context.push ctx;
  prog
  |> List.iter (fun stmt ->
         match stmt with
         | Function { name; params; return; body } ->
             let fun_ty = Type.FunctionType { params; return } in
             bind_name_to_type ctx name fun_ty (Right stmt);
             Context.push ctx;
             List.iter
               (fun (name, ty) -> bind_name_to_type ctx name ty (Right stmt))
               params;
             infer_body ctx body;
             Context.pop ctx
         | _ ->
             raise
               (general_error ~msg:"only functions can be written at top level"
                  (Right stmt)))
