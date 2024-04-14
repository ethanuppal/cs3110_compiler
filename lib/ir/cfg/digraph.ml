type vertex_id = int

type 'a vertex = {
  adj : vertex_id BatDynArray.t;
  value : 'a;
}

(** AF: equivalent to the nested list
    [adj = {[v1; v2; v3; ...]; value = val1}; {adj = [v4; v5; v6; ,,,]; value = val2}; ...]
    is the directed graph where [v1] has edges to the vertices indexed by [v1],
    [v2], and [v3], [v2] has edges to those indexed by [v4], [v5], [v6], and so
    on, and where vertex index [i] corresponds to the vertex at index [i] in the
    list. *)
type 'a t = 'a vertex BatDynArray.t

let vertex_count = BatDynArray.length
let int_of_vertex_id = Util.id
let vertex_id_of_int = Util.id
let vertices_of dg = Seq.init (BatDynArray.length dg) Util.id

let rep_ok dg =
  if Rep_ok.check then
    if vertex_count dg <> Seq.length (vertices_of dg) then failwith "rep_ok";
  dg

let make () = BatDynArray.make 16 |> rep_ok
let get dg v = (BatDynArray.get (rep_ok dg) v).value

let add_vertex dg value =
  let result = BatDynArray.length (rep_ok dg) in
  BatDynArray.add dg { adj = BatDynArray.make 16; value };
  rep_ok dg |> ignore;
  result

let add_edge dg v1 = (BatDynArray.get (rep_ok dg) v1).adj |> BatDynArray.add
