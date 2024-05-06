let emit_ir section =
  ignore section;
  failwith "i hate reg alloc"

let emit_bb section cfg bb =
  let ir = Basic_block.to_list bb in
  let jumps = Cfg.out_edges cfg bb in
  ignore jumps;
  List.iter (emit_ir section) ir;
  failwith "emit jumps"

let emit section cfg = Cfg.blocks_of cfg |> List.iter (emit_bb section cfg)
