type ir_array = Ir.t BatDynArray.t

type t = {
  (* label : Label.t; *)
  contents : ir_array;
  branch : Branch_condition.t;
}

let make condition = { contents = BatDynArray.create (); branch = condition }
let add basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
