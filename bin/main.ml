open X86ISTMB
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

let test () =
  let ir =
    [
      Ir.Instr.Assign (Ir.Variable.make 0, Ir.Operand.make_const 1);
      Ir.Instr.Assign (Ir.Variable.make 1, Ir.Operand.make_const 2);
      Ir.Instr.Add
        (Ir.Variable.make 2, Ir.Operand.make_var 0, Ir.Operand.make_var 1);
    ]
  in
  let bb = Ir.BasicBlock.make () in
  List.iter (Ir.BasicBlock.add bb) ir;
  Printf.printf "%s:\n" (Ir.BasicBlock.label_of bb |> Ir.Label.name_of);
  List.iter (Ir.Instr.to_string >> Printf.printf "  %s\n") ir

let file_driver path flags =
  let source = Util.read_file path in
  try
    let statements = Frontend.ParseLex.lex_and_parse source in
    ignore statements;
    if List.mem Cli.OnlyIR flags then test ()
    else failwith "compiler not done yet"
  with Frontend.ParseLex.ParseError msg -> print_error (msg ^ "\n")

let () =
  match Sys.argv |> Cli.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | File { prog = _; path; flags } -> file_driver path flags
  | Error { prog; msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error
