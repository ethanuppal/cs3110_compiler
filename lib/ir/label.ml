open Id

type t = {
  id : id;
  name : string;
}

module LabelMap = Hashtbl.Make (String)

(** TODO: make work with multiple files and stuff *)
let label_map : id LabelMap.t = LabelMap.create 16

let make gen name =
  let label = { id = Gen.next gen; name } in
  if LabelMap.find_opt label_map name <> None then
    failwith "see todo above label_map";
  LabelMap.add label_map label.name label.id;
  label

let make_plain gen =
  let id = Gen.next gen in
  let name = ".L" ^ string_of_int id in
  let label = { id; name } in
  if LabelMap.find_opt label_map name <> None then
    failwith "see todo above label_map";
  LabelMap.add label_map label.name label.id;
  label

let resolve name = { id = LabelMap.find label_map name; name }
let id_of label = label.id
let name_of label = label.name
