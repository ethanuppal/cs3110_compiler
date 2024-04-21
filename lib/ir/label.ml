open Id

type t = {
  id : id;
  name : string;
  is_external : bool;
  is_global : bool;
}

module LabelMap = Hashtbl.Make (String)

(** TODO: make work with multiple files and stuff *)
let label_map : t LabelMap.t = LabelMap.create 16

let make_symbol ?(is_external = false) ?(is_global = true) gen name =
  let name = "_" ^ name in
  let label = { id = Gen.next gen; name; is_external; is_global } in
  if LabelMap.find_opt label_map name <> None then
    failwith "see todo above lib/label.ml:label_map";
  LabelMap.add label_map label.name label;
  label

let make_location gen =
  let id = Gen.next gen in
  let name = ".L" ^ string_of_int (Gen.id_of gen) ^ "_" ^ string_of_int id in
  let label = { id; name; is_external = false; is_global = false } in
  if LabelMap.find_opt label_map name <> None then
    failwith "see todo above label_map";
  LabelMap.add label_map label.name label;
  label

let resolve = LabelMap.find label_map
let id_of label = label.id
let name_of label = label.name
let is_external label = label.is_external
let is_global label = label.is_global
