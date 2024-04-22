open Util

let emit_ir section =
  ignore section;
  failwith "i hate reg alloc"

let emit_bb section cfg bb =
  let ir = Basic_block.to_list cfg in
  let jumps = Cfg.edges_from bb in
  List.iter (emit_ir section) ir;
  failwith "emit jumps"

let emit section cfg = Cfg.blocks_of cfg |> List.iter (emit_bb section cfg)
