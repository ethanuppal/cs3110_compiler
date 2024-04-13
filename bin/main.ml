open Cs3110_compiler
open Util

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

let print_bb bb =
  Printf.printf "%s:\n" (Basic_block.label_of bb |> Label.name_of);
  List.iter (Ir.to_string >> Printf.printf "  %s\n") (Basic_block.to_list bb)

let test () =
  let cfg = Control_flow_graph.make () in
  let entry = Control_flow_graph.entry cfg in
  let symbol_gen = Id.Gen.make () in
  let part2 = Control_flow_graph.label_for_new_bb cfg in
  let part3 = Control_flow_graph.label_for_new_bb cfg in
  let ir1 =
    [|
      Ir.Assign (Ir.var 0, Ir.const 4);
      Ir.Jump (part2, Ir.BranchCondition.Equal (Ir.var_op 0, Ir.const 0));
    |]
  in
  let ir2 =
    [|
      Ir.Param (Ir.const 5);
      Ir.Call (Label.make_symbol symbol_gen "print");
      Ir.Jump (part3, Ir.BranchCondition.Unconditional);
    |]
  in
  let ir3 =
    [| Ir.Jump (Basic_block.label_of entry, Ir.BranchCondition.Unconditional) |]
  in
  Control_flow_graph.insert_ir cfg entry ir1.(0) |> ignore;
  let bb2 = Control_flow_graph.insert_ir cfg entry ir1.(1) |> Option.get in
  Control_flow_graph.insert_ir cfg bb2 ir2.(0) |> ignore;
  Control_flow_graph.insert_ir cfg bb2 ir2.(1) |> ignore;
  let bb3 = Control_flow_graph.insert_ir cfg bb2 ir2.(2) |> Option.get in
  Control_flow_graph.insert_ir cfg bb3 ir3.(0) |> ignore;
  List.iter print_bb (Control_flow_graph.to_list cfg)

let file_driver path flags =
  let source = Util.read_file path in
  try
    let statements = Parse_lex.lex_and_parse source in
    ignore statements;
    if List.mem Cli.OnlyIR flags then test ()
    else failwith "compiler not done yet"
  with Parse_lex.ParseError msg -> print_error (msg ^ "\n")

let () =
  match Sys.argv |> Cli.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | File { prog = _; path; flags } -> file_driver path flags
  | Error { prog; msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error
