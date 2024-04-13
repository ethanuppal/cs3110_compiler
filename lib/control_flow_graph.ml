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

let make_bb_label cfg = Label.make_location cfg.gen

(** [retrieve cfg label] is the basic block labeled [label] in [cfg]. If such a
    basic block does not exist, a new one is created in [cfg]. Runs in amortized
    [O(1)] time if called with every basic block created over the course of the
    control flow graph.

    The basic block label identifiers are kept in sync with their vertex
    identifiers to the point of integer equality. By the invariant on
    [Digraph]s, the vertex identifier is related to the length of the array such
    that if there are less vertices than one more than the integral form of the
    basic block identifier, we can simply add new vertices until we obtain that
    count, and then return the most recently-added vertex. Of course, the loop
    does not execute if the basic block already exists in the graph. *)
let retrieve cfg label =
  let cfg = rep_ok cfg in
  let bb_label_id = Label.id_of label in
  while Digraph.vertex_count cfg.graph < bb_label_id + 1 do
    ignore (Digraph.add_vertex cfg.graph (Basic_block.make_for_label label))
  done;
  Digraph.get (rep_ok cfg).graph (Digraph.vertex_id_of_int bb_label_id)

let insert_ir cfg bb ir =
  let open Util in
  let bb_to_vertex_id =
    Basic_block.label_of >> Label.id_of >> Digraph.vertex_id_of_int
  in
  let cfg = rep_ok cfg in
  Basic_block.add bb ir;
  match ir with
  | Ir.Jump (label, _) ->
      let bb2 = retrieve cfg label in
      Digraph.add_edge cfg.graph (bb_to_vertex_id bb) (bb_to_vertex_id bb2);
      Some bb2
  | _ -> None

let to_list cfg =
  let cfg = rep_ok cfg in
  Digraph.vertices_of cfg.graph
  |> Seq.map (Digraph.get cfg.graph)
  |> List.of_seq
