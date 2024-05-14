open Util
module Scope = Hashtbl.Make (String)

type 'a t = {
  mutable scopes : 'a Scope.t list;
  mutable namespace : string list;
}

let make () = { scopes = []; namespace = [] }
let stack_size ctx = List.length ctx.scopes
let is_empty ctx = List.is_empty ctx.scopes
let push ctx = ctx.scopes <- Scope.create 16 :: ctx.scopes
let pop ctx = ctx.scopes <- List.tl ctx.scopes
let top ctx = List.hd ctx.scopes
let insert ctx = Scope.replace (top ctx)

let get ctx key =
  let rec get_aux lst =
    match lst with
    | [] -> None
    | scope :: rest -> (
        match Scope.find_opt scope key with
        | None -> get_aux rest
        | Some value -> Some value)
  in
  get_aux ctx.scopes

let get_local ctx key =
  if is_empty ctx then None else Scope.find_opt (top ctx) key

let to_list ctx = ctx.scopes |> List.map (Scope.to_seq >> List.of_seq)
let add_namespace ctx name = ctx.namespace <- name :: ctx.namespace
let pop_namespace ctx = ctx.namespace <- List.tl ctx.namespace
let in_namespace ctx symbol = symbol :: ctx.namespace |> List.rev
