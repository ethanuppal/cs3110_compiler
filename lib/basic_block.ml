type ir_array = Ir.t BatDynArray.t

type t = {
  label : Label.t;
  contents : ir_array;
}

let gen = Id.Gen.make ()
let make () = { label = Label.make_plain gen; contents = BatDynArray.create () }
let label_of basic_block = basic_block.label
let add basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents