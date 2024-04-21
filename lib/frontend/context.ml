module Scope = Hashtbl.Make (String)

type 'a t = 'a Scope.t BatDynArray.t

let make () = BatDynArray.make 16
let stack_size = BatDynArray.length
let is_empty = BatDynArray.empty
let push ctx = BatDynArray.add ctx (Scope.create 16)
let pop = BatDynArray.delete_last

(** [top ctx] is the scope at the top of the stack.

    Requires: [not (is_empty ctx)]. *)
let top = BatDynArray.last

let insert ctx = Scope.add (top ctx)

let get ctx key =
  let rec get_aux i =
    if i = -1 then None
    else
      let scope = BatDynArray.get ctx i in
      match Scope.find_opt scope key with
      | None -> get_aux (i - 1)
      | Some value -> Some value
  in
  get_aux (stack_size ctx - 1)
