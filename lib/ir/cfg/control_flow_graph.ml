open Id

(** AF: [{graph; entry}] is a control flow graph with vertices and edges
    identical to that in the directed graph and with entry point [entry].

    RI: Let [cfg = {graph; entry}] be of type [t]. Then, [entry = entry cfg] is
    in [graph] and has an in-degree of zero. For each basic block [bb] in
    [graph], the handle of type [Digraph.vertex_id] for [bb], which we let
    [bb_id], is equal to [bb |> Basic_block.label_of |> Label.id_of]. This
    invariant is enabled because of the congruence between the specifications of
    [Basic_block.make_for_label]/[Id.Gen.next] and [Digraph.int_of_vertex_id]. *)
type t = {
  graph : Basic_block.t Digraph.t;
  entry : Digraph.vertex_id;
  gen : Gen.t;
}

(** [rep_ok cfg = cfg] if and only if the representation invariant is not
    violated. *)
let rep_ok cfg =
  if Rep_ok.check then
    Digraph.vertices_of cfg.graph
    |> Seq.iter (fun bb_id ->
           if
             Digraph.get cfg.graph bb_id
             |> Basic_block.label_of |> Label.id_of
             <> Digraph.int_of_vertex_id bb_id
           then failwith "rep_ok");
  cfg

let make () =
  let graph = Digraph.make () in
  let gen = Gen.make () in
  let entry = Digraph.add_vertex graph (Basic_block.make gen) in
  rep_ok { graph; entry; gen }

let entry cfg =
  let cfg = rep_ok cfg in
  Digraph.get cfg.graph cfg.entry

let label_for_new_bb cfg =
  let label = Label.make_location cfg.gen in
  let bb = Basic_block.make_for_label label in
  Digraph.add_vertex cfg.graph bb |> ignore;
  label

let insert_ir cfg bb ir =
  let open Util in
  let bb_to_vertex_id =
    Basic_block.label_of >> Label.id_of >> Digraph.vertex_id_of_int
  in
  let cfg = rep_ok cfg in
  Basic_block.add bb ir;
  match ir with
  | Ir.Jump (label, _) ->
      let bb2 =
        Digraph.get cfg.graph (Digraph.vertex_id_of_int (Label.id_of label))
      in
      Digraph.add_edge cfg.graph (bb_to_vertex_id bb) (bb_to_vertex_id bb2);
      Some bb2
  | _ -> None

let to_list cfg =
  let cfg = rep_ok cfg in
  Digraph.vertices_of cfg.graph
  |> Seq.map (Digraph.get cfg.graph)
  |> List.of_seq
