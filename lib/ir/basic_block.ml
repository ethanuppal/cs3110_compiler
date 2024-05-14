(** TODO: needs tests *)

let bb_gen = Id.Gen.make ()

type t = {
  id : Id.id;
  contents : (Ir.t * int) BatDynArray.t;
  mutable condition : Branch_condition.t;
}

let make () =
  {
    id = Id.Gen.next bb_gen;
    contents = BatDynArray.make 4;
    condition = Branch_condition.Never;
  }

let id_of bb = bb.id
let length_of bb = BatDynArray.length bb.contents
let condition_of bb = bb.condition
let set_condition bb cond = bb.condition <- cond

let add_ir bb ir =
  let i = length_of bb in
  BatDynArray.add bb.contents (ir, i)

let get_ir bb idx = BatDynArray.get bb.contents idx |> fst
let get_orig_idx bb idx = BatDynArray.get bb.contents idx |> snd
let set_ir bb idx ir = BatDynArray.set bb.contents idx (ir, get_orig_idx bb idx)
let rem_ir bb idx = BatDynArray.remove_at idx bb.contents
let to_list bb = BatDynArray.to_list bb.contents |> List.map fst
let label_for bb = Printf.sprintf ".L_BB%d" (id_of bb |> Id.int_of)
let equal bb1 bb2 = Id.equal bb1.id bb2.id
let hash bb = Id.int_of bb.id |> Int.hash

let to_string bb =
  label_for bb
  ^ BatDynArray.fold_left
      (fun acc (ir, _) -> acc ^ "\n  " ^ Ir.to_string ir)
      "" bb.contents
  ^ "\n  branch if "
  ^ Branch_condition.to_string bb.condition
