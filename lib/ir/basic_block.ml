(** TODO: needs tests *)

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.id;
  contents : Ir.t BatDynArray.t;
  mutable condition : Branch_condition.t;
}

let make () =
  {
    id = Id.Gen.next bb_gen;
    contents = BatDynArray.make 4;
    condition = Branch_condition.Never;
  }

let id_of basic_block = basic_block.id
let length_of bb = BatDynArray.length bb.contents
let condition_of bb = bb.condition
let set_condition bb cond = bb.condition <- cond
let add_ir basic_block ir = BatDynArray.add basic_block.contents ir
let get_ir basic_block index = BatDynArray.get basic_block.contents index
let set_ir basic_block index ir = BatDynArray.set basic_block.contents index ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
let equal bb1 bb2 = bb1.id = bb2.id
let hash bb = Id.int_of bb.id |> Int.hash
let as_view bb = Util.ArrayView.from_bat_dyn_arr bb.contents

let to_string bb =
  Printf.sprintf ".L%d:" (id_of bb |> Id.int_of)
  ^ BatDynArray.fold_left
      (fun acc ir -> acc ^ "\n  " ^ Ir.to_string ir)
      "" bb.contents
  ^ "\n  br "
  ^ Branch_condition.to_string bb.condition
