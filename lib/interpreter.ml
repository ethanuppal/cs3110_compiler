type interpreter_mode =
  | File
  | REPL

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

let rec interpreter_eval (i : t' ref) : Ast.expr -> Value.t = function
  | Var name -> (
      match interpreter_lookup i name with
      | None ->
          failwith
            "name couldn't be resolved, make better exception handling system \
             here"
      | Some value -> value)
  | Const const ->
      if const = 69 && funny_mode = FunnyLol then print_endline "Nice";
      const
  | Infix { lhs; op; rhs } -> (
      let eval_lhs = interpreter_eval i lhs in
      let eval_rhs = interpreter_eval i rhs in
      match op with
      | Plus ->
          if eval_lhs = 9 && eval_rhs = 10 && funny_mode = FunnyLol then 21
          else eval_lhs + eval_rhs
      | Minus -> eval_lhs - eval_rhs
      | Times -> eval_lhs * eval_rhs)

let interpreter_step (i : t' ref) : Ast.stmt -> unit = function
  | Declaration (name, expr) ->
      let value = interpreter_eval i expr in
      let scope =
        BatDynArray.get !i.scopes (BatDynArray.length !i.scopes - 1)
      in
      Scope.store scope name value;
      if !i.mode = REPL then Printf.printf "[bind %s = %d]\n" name value
  | Print expr ->
      let value = interpreter_eval i expr in
      if !i.mode = REPL then Printf.printf "[print]\n";
      print_endline (Value.to_string value)

(* Creates a new interpreter with one empty scope *)
let create () : t =
  let i = ref { scopes = BatDynArray.create (); mode = File } in
  BatDynArray.add !i.scopes (Scope.empty ());
  {
    dump = interpreter_dump i;
    step = interpreter_step i;
    set_mode = (fun mode -> !i.mode <- mode);
  }
