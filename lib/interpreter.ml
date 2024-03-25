type t = {
  dump : unit -> unit;
  step : Ast.stmt -> unit;
}

type t' = { scopes : Scope.t BatDynArray.t }

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
    if idx = -1 then () else scope_dump (BatDynArray.get !i.scopes idx);
    dump_aux (idx - 1)
  in
  dump_aux (BatDynArray.length !i.scopes - 1)

let interpreter_lookup (i : t' ref) name =
  let rec lookup_aux idx =
    if idx = -1 then None
    else
      match Scope.lookup name (BatDynArray.get !i.scopes idx) with
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
  | Const const -> const
  | Infix { lhs; op; rhs } -> (
      let eval_lhs = interpreter_eval i lhs in
      let eval_rhs = interpreter_eval i rhs in
      match op with
      | Plus -> eval_lhs + eval_rhs
      | Minus -> eval_lhs - eval_rhs
      | Times -> eval_lhs * eval_rhs)

let interpreter_step (i : t' ref) : Ast.stmt -> unit = function
  | Declaration (name, expr) ->
      let value = interpreter_eval i expr in
      let scope =
        BatDynArray.get !i.scopes (BatDynArray.length !i.scopes - 1)
      in
      Scope.store scope name value
  | Print expr ->
      let value = interpreter_eval i expr in
      print_endline (Value.to_string value)

let create () : t =
  let i = ref { scopes = BatDynArray.create () } in
  BatDynArray.add !i.scopes (Scope.empty ());
  { dump = interpreter_dump i; step = interpreter_step i }
