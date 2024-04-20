(** TODO: needs tests *)

type ir_array = Ir.t BatDynArray.t

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.id;
  contents : ir_array;
  mutable condition : Branch_condition.t;
}

let make () =
  {
    id = Id.Gen.next bb_gen;
    contents = BatDynArray.create ();
    condition = Branch_condition.Never;
  }

let id_of basic_block = basic_block.id
let condition_of bb = bb.condition
let set_condition bb cond = bb.condition <- cond
let add_ir basic_block ir = BatDynArray.add basic_block.contents ir
let to_list basic_block = BatDynArray.to_list basic_block.contents
let equal bb1 bb2 = bb1.id = bb2.id
let hash bb = bb.id
