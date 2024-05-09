open Util
module Scope = Hashtbl.Make (String)

type 'a t = 'a Scope.t list ref

let make () = ref []
let stack_size ctx = List.length !ctx
let is_empty ctx = List.is_empty !ctx
let push ctx = ctx := Scope.create 16 :: !ctx
let pop ctx = ctx := List.tl !ctx
let top ctx = List.hd !ctx
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
  get_aux !ctx

let get_local ctx key =
  if is_empty ctx then None else Scope.find_opt (top ctx) key

let to_list ctx = !ctx |> List.map (Scope.to_seq >> List.of_seq)
