type id = int
type ir_array = Ir.t BatDynArray.t

type t = {
  id : id;
  contents : ir_array;
}

let id_gen =
  let id = ref 0 in
  fun () ->
    let result = !id in
    id := !id + 1;
    result

let make () = { id = id_gen (); contents = BatDynArray.create () }
let id_of basic_block = basic_block.id
let label_of basic_block = ".L" ^ string_of_int basic_block.id
let add basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
