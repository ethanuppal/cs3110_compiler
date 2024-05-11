let print_error = Printf.eprintf "error: %s"

let print_help prog =
  let open Printf in
  printf "%s\n" Meta.get.description;
  printf "\n";
  printf "Usage: %s [-h|-v]\n" prog;
  printf "   or: %s FILE [-g][-O]\n" prog;
  printf "\n";
  printf "-h,--help         prints this info\n";
  printf "-v,--version      prints version info\n";
  printf "-g,--gen          only produces IR\n";
  printf "-O,--optimize     runs optimizations\n";
  printf "-c,--compile      only produces object files\n";
  ignore ()

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let compile paths _ =
  Printf.printf "assumes [paths] has one file, ignores flags\n";
  let source_path = List.hd paths in
  let source = Util.read_file source_path in
  try
    let statements = Parse_lex.lex_and_parse ~filename:source_path source in
    Analysis.infer statements;
    let cfgs = Ir_gen.generate statements in
    let main_cfg = List.hd cfgs in
    let liveliness_analysis = Liveliness.analysis_of main_cfg in
    let instr_ordering = InstrOrdering.make main_cfg in
    let regalloc =
      Regalloc.allocate_for main_cfg liveliness_analysis instr_ordering
    in
    Asm_emit.emit section
    (* let simulator = Ir_sim.make () in Ir_sim.run simulator main_cfg;
       print_string (Ir_sim.output_of simulator) *)
  with Parse_lex.ParserError msg -> print_error (msg ^ "\n")

let main args =
  let parse = Cli.parse args in
  match parse.action with
  | Help -> print_help parse.prog
  | Version -> print_version ()
  | Compile { paths; flags } -> compile paths flags
  | Error { msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg parse.prog |> print_error
