type ir_array = Ir.t BatDynArray.t

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.id;
  contents : ir_array; (* branch : Branch_condition.t; *)
}

let make condition =
  ignore condition;
  {
    id = Id.Gen.next bb_gen;
    contents = BatDynArray.create ();
    (* branch = condition; *)
  }

let id_of basic_block = basic_block.id
let add basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
