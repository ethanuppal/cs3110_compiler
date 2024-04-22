(** TODO: needs tests *)

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.t;
  mutable contents : Ir.t list;
      (* TODO: this should be BatDynArr.t because otherwise passes will be
         scuffed *)
  mutable condition : Branch_condition.t;
}

let make () =
  { id = Id.Gen.next bb_gen; contents = []; condition = Branch_condition.Never }

let id_of basic_block = basic_block.id
let condition_of bb = bb.condition
let set_condition bb cond = bb.condition <- cond
let add_ir basic_block ir = basic_block.contents <- ir :: basic_block.contents
let to_list basic_block = List.rev basic_block.contents
let equal bb1 bb2 = Id.equal bb1.id bb2.id
let hash bb = Id.hash bb.id
