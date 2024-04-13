open Ast

exception
  TypeInferenceError of {
    domain : string;
    symbol : string option;
    ast : (expr, stmt) Either.t;
    unify : (Type.t * Type.t) option;
  }

let () =
  Printexc.register_printer (function
    | TypeInferenceError { domain; symbol; ast = _; unify } ->
        let result = Printf.sprintf "Type %s error" domain in
        let result =
          match symbol with
          | None -> result
          | Some symbol -> result ^ " (symbol='" ^ symbol ^ "')"
        in
        let result =
          match unify with
          | None -> result
          | Some (lhs, rhs) ->
              result
              ^ Printf.sprintf ": attempt to unify %s and %s"
                  (Type.to_string lhs) (Type.to_string rhs)
        in
        Some result
    | _ -> None)

(** After [infer_expr ctx hint expr], [expr] will be assigned a type based on
    [hint] and [ctx].

    @raise TypeInferenceError on failure. *)
let rec infer_expr ctx hint expr =
  match expr with
  | Var var -> var.ty <- Context.get ctx var.name
  | ConstInt _ -> (
      match hint with
      | None | Some (Type.Primitive Int63) | Some Any -> ()
      | Some other ->
          raise
            (TypeInferenceError
               {
                 domain = "unification";
                 symbol = None;
                 ast = Left expr;
                 unify = Some (other, Type.Primitive Int63);
               }))
  | ConstBool _ -> (
      match hint with
      | None | Some (Type.Primitive Bool) | Some Any -> ()
      | Some other ->
          raise
            (TypeInferenceError
               {
                 domain = "unification";
                 symbol = None;
                 ast = Left expr;
                 unify = Some (other, Type.Primitive Bool);
               }))
  | Infix infix -> (
      match infix.op with
      | Plus | Minus | Times | Divide | Mod ->
          infer_expr ctx (Some Type.int_prim_type) infix.lhs;
          infer_expr ctx (Some Type.int_prim_type) infix.rhs;
          infix.ty <- Some Type.int_prim_type)
  | Prefix prefix -> (
      match prefix.op with
      | Plus | Minus ->
          infer_expr ctx (Some Type.int_prim_type) prefix.rhs;
          prefix.ty <- Some Type.int_prim_type
      | Times ->
          infer_expr ctx (Some Type.(Pointer any_type)) prefix.rhs;
          prefix.ty <- Option.map Type.deref (type_of_expr prefix.rhs)
      | _ ->
          raise
            (TypeInferenceError
               {
                 domain = "combination";
                 symbol = None;
                 ast = Left expr;
                 unify = None;
               }))
  | FunctionExpr _ -> ()

(** @raise TypeInferenceError on failure. *)
let infer_stmt ctx stmt =
  match stmt with
  | Declaration { name; hint; expr } ->
      (infer_expr ctx hint expr;
       match (hint, type_of_expr expr) with
       | None, None ->
           raise
             (TypeInferenceError
                {
                  domain = "resolution";
                  symbol = Some name;
                  ast = Right stmt;
                  unify = None;
                })
       | Some _, None | None, Some _ -> ()
       | Some t1, Some t2 ->
           if t1 <> t2 && t1 <> Type.any_type then
             raise
               (TypeInferenceError
                  {
                    domain = "unification";
                    symbol = Some name;
                    ast = Right stmt;
                    unify = Some (t1, t2);
                  }));
      Context.insert ctx name (type_of_expr expr |> Option.get)
  | _ -> ()

let infer prog =
  let ctx = Context.make () in
  Context.push ctx;
  List.iter (infer_stmt ctx) prog;
  Context.pop ctx
