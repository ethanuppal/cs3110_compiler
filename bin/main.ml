open X86ISTMB

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
  ignore ()

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let show_ir statements =
  let ir = Ir_gen.generate statements in
  let main_cfg = List.hd ir in
  let blocks = Cfg.blocks main_cfg in
  List.iter
    (fun b ->
      let lst = Basic_block.to_list b in
      Printf.printf "Block %i:\n" (Basic_block.id_of b);
      List.iter (fun bir -> print_endline (Ir.to_string bir)) lst;
      print_newline ())
    blocks;
  let edges = Cfg.edges main_cfg in
  List.iter
    (fun (b1, e, b2) ->
      Printf.printf "Block %i" (Basic_block.id_of b1);
      Printf.printf " --%s (%s)--> "
        (Basic_block.condition_of b1 |> Branch_condition.to_string)
        (if e then "t" else "f");
      Printf.printf "Block %i" (Basic_block.id_of b2);
      print_newline ())
    edges

let file_driver path flags =
  let source = Util.read_file path in
  try
    let statements = Parse_lex.lex_and_parse source in
    ignore statements;
    if List.mem Cli.OnlyIR flags then show_ir statements
    else failwith "compiler not done yet"
  with Parse_lex.ParseError msg -> print_error (msg ^ "\n")

let () =
  match Sys.argv |> Cli.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | File { prog = _; path; flags } -> file_driver path flags
  | Error { prog; msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error
