type ir_array = Ir.t BatDynArray.t

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.id;
  contents : ir_array;
  condition : Branch_condition.t;
}

let make condition =
  { id = Id.Gen.next bb_gen; contents = BatDynArray.create (); condition }

let id_of basic_block = basic_block.id
let condition_of bb = bb.condition
let add basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
let equal bb1 bb2 = bb1.id = bb2.id
let hash bb = bb.id
