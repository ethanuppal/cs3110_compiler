type interpreter_mode =
  | File
  | REPL
  | Text of string ref

(** The duplicated public interface from [interpreter.mli]. Please ensure that
    these two types are in sync. *)
type t = {
  dump : unit -> unit;
  step : Ast.stmt -> unit;
  set_mode : interpreter_mode -> unit;
}

(** The internal implementation of the interpreter. *)
type t' = {
  scopes : Scope.t BatDynArray.t;
  mutable mode : interpreter_mode;
}

(** [NameResolutionError name] means that [name] could not be resolved in any
    available scope. *)
exception NameResolutionError of string

(** [NameRedefinitionError name] is raised with [name] has already been defined
    in the scope. *)
exception NameRedefinitionError of string

exception
  OperatorMisuseError of {
    op : Ast.op;
    env : string;
  }

module FunnyMode = struct
  (** The funniness aspect of the interpreter's semantics. *)
  type t =
    | FunnyLol
    | VeryBoring
end

(** The current funniness aspect. *)
let funny_mode = FunnyMode.VeryBoring

let interpreter_dump (i : t' ref) () =
  let open Util in
  let scope_dump =
    Scope.bindings
    >> Seq.fold_left (fun acc elem -> elem :: acc) []
    >> List.map (fun (name, _) -> Printf.sprintf "(%s = <val>)" name)
    >> String.concat ", "
    >> (fun x -> "[" ^ x ^ "]")
    >> print_endline
  in
  let rec dump_aux idx =
    if idx = -1 then ()
    else (
      scope_dump (BatDynArray.get !i.scopes idx);
      dump_aux (idx - 1))
  in
  dump_aux (BatDynArray.length !i.scopes - 1)

let interpreter_lookup (i : t' ref) name =
  let rec lookup_aux idx =
    if idx = -1 then None
    else
      match Scope.lookup (BatDynArray.get !i.scopes idx) name with
      | None -> lookup_aux (idx - 1)
      | binding -> binding
  in
  lookup_aux (BatDynArray.length !i.scopes - 1)

let interpreter_push (i : t' ref) = BatDynArray.add !i.scopes (Scope.empty ())
let intepreter_pop (i : t' ref) = BatDynArray.delete_last !i.scopes

let rec interpreter_eval (i : t' ref) : Ast.expr -> Value.t = function
  | Var { name; _ } -> (
      match interpreter_lookup i name with
      | None -> raise (NameResolutionError name)
      | Some value -> value)
  | ConstInt const ->
      if const = 69 && funny_mode = FunnyLol then print_endline "Nice";
      Int const
  | Infix { lhs; op; rhs; _ } ->
      Int
        (let eval_lhs = interpreter_eval i lhs |> Value.as_int in
         let eval_rhs = interpreter_eval i rhs |> Value.as_int in
         match op with
         | Plus ->
             if eval_lhs = 9 && eval_rhs = 10 && funny_mode = FunnyLol then 21
             else eval_lhs + eval_rhs
         | Minus -> eval_lhs - eval_rhs
         | Times -> eval_lhs * eval_rhs
         | Divide -> eval_lhs / eval_rhs
         | Mod -> eval_lhs mod eval_rhs)
  | Prefix { op; rhs; _ } ->
      Int
        (let eval_rhs = interpreter_eval i rhs |> Value.as_int in
         match op with
         | Plus -> eval_rhs
         | Minus -> -eval_rhs
         | Times -> failwith "pointers not impl yet"
         | _ -> raise (OperatorMisuseError { op; env = "prefix" }))
  | FunctionExpr { body } -> FunctionValue { body }
  | _ -> failwith "interpreter error: missing case for an expr type"

let rec interpreter_step (i : t' ref) (stmt : Ast.stmt) : unit =
  try
    let update_binding enforce_fresh name expr =
      let scope =
        BatDynArray.get !i.scopes (BatDynArray.length !i.scopes - 1)
      in
      if enforce_fresh then (
        if Scope.lookup scope name <> None then
          raise (NameRedefinitionError name))
      else if Scope.lookup scope name = None then
        raise (NameResolutionError name);
      let value = interpreter_eval i expr in
      Scope.store scope name value;
      if !i.mode = REPL then
        Printf.printf "[bind %s = %s]\n" name (Value.to_string value)
    in
    match stmt with
    | Call name -> (
        match interpreter_lookup i name with
        | None -> raise (NameResolutionError name)
        | Some func_value ->
            let func_body = func_value |> Value.as_func in
            interpreter_push i;
            List.iter (interpreter_step i) func_body;
            intepreter_pop i)
    | Declaration { name; hint = _; expr } -> update_binding true name expr
    | Assignment (name, expr) -> update_binding false name expr
    | Function { name; body } ->
        update_binding true name (FunctionExpr { body })
    | Print expr -> (
        let value = interpreter_eval i expr in
        if !i.mode = REPL then Printf.printf "[print]\n";
        match !i.mode with
        | Text output_ref ->
            output_ref := !output_ref ^ Value.to_string value ^ "\n"
        | _ -> print_endline (Value.to_string value))
  with
  | NameResolutionError name ->
      Printf.eprintf "NameResolutionError: '%s' could not be resolved\n%!" name
  | NameRedefinitionError name ->
      Printf.eprintf "NameRedefinitionError: invalid redefinition of '%s'\n%!"
        name
  | Value.TypeError { value; ctx } ->
      Printf.eprintf "TypeError: illegal type operation: %s on '%s'\n%!" ctx
        (Value.to_string value)

(* Creates a new interpreter with one empty scope *)
let create () : t =
  let i = ref { scopes = BatDynArray.create (); mode = File } in
  BatDynArray.add !i.scopes (Scope.empty ());
  {
    dump = interpreter_dump i;
    step = interpreter_step i;
    set_mode = (fun mode -> !i.mode <- mode);
  }
