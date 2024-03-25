type t = {
  dump : unit -> unit;
  lookup : string -> Value.t option;
}

type t' = { scopes : Scope.t BatDynArray.t }

let interpreter_dump i =
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

let interpreter_lookup i name =
  let rec lookup_aux idx =
    if idx = -1 then None
    else
      match Scope.lookup name (BatDynArray.get !i.scopes idx) with
      | None -> lookup_aux (idx - 1)
      | binding -> binding
  in
  lookup_aux (BatDynArray.length !i.scopes - 1)

(* let interpreter_eval i = let eval_aux =

   let interpreter_run i stm = failwith "not impl" *)

let create () : t =
  let i = ref { scopes = BatDynArray.create () } in
  {
    dump = interpreter_dump i;
    lookup = interpreter_lookup i;
    (* run = interpreter_run i; *)
  }
