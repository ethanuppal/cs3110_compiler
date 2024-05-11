let runtime_lib_loc = Util.merge_paths [ Project_root.path; "lib/runtime" ]
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

let compile paths _ build_dir_loc =
  Printf.printf "[DEBUG] assumes [paths] has one file, ignores flags\n";
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
    let text_section = Asm.Section.make "text" 16 in
    Asm_emit.emit_preamble ~text:text_section;
    Asm_emit.emit_cfg ~text:text_section main_cfg regalloc;
    let asm_file = Asm.AssemblyFile.make () in
    Asm.AssemblyFile.add asm_file text_section;
    let asm_output_path =
      BatFilename.(source_path |> basename |> chop_extension) ^ ".nasm"
    in
    let build_dir =
      Util.merge_paths
        [
          (match build_dir_loc with
          | Some loc -> loc
          | None -> ".");
          "build_dir";
        ]
    in
    if Sys.command (Printf.sprintf "mkdir -p %s" build_dir) <> 0 then
      failwith "could not create folder build_dir/ in current directory";
    if Sys.command (Printf.sprintf "rm -f %s/*" build_dir) <> 0 then
      failwith "could not remove old build_dir/ contents";
    Util.write_file
      (Util.merge_paths [ build_dir; asm_output_path ])
      (Asm.AssemblyFile.to_nasm asm_file);
    Util.write_file
      (Util.merge_paths [ build_dir; "Makefile" ])
      (Printf.sprintf
         "build:\n\
          \t@nasm -f macho64 %s -o build.o\n\
          \t@clang build.o %s/* -o a.out\n"
         asm_output_path runtime_lib_loc);
    let uname = Util.get_command_output "uname" in
    let processor = Util.get_command_output "uname -p" in
    let cmd_prefix =
      if
        Util.contains_substring uname "Darwin"
        && Util.contains_substring processor "arm"
      then "arch -x86_64 "
      else ""
    in
    Util.write_file
      (Util.merge_paths [ build_dir; "cmd_prefix.txt" ])
      cmd_prefix;
    (* Util.write_file "build_dir/exec_helper" ("#!/bin/sh\n" ^ cmd_prefix ^
       "./a.out\n"); *)
    if
      Sys.command
        (Printf.sprintf "cd %s/ && " build_dir
        ^ cmd_prefix ^ "make build 2>/dev/null")
      <> 0
    then failwith "compilation failed";
    Printf.printf "==> Wrote build files to %s\n" build_dir;
    Printf.printf
      "==> You can run the executable with %s, prefixing with Rosetta as \
       appropriate\n"
      (Util.merge_paths [ build_dir; "a.out" ])
    (* let simulator = Ir_sim.make () in Ir_sim.run simulator main_cfg;
       print_string (Ir_sim.output_of simulator) *)
  with Parse_lex.ParserError msg -> print_error (msg ^ "\n")

let main args =
  let parse = Cli.parse args in
  match parse.action with
  | Help -> print_help parse.prog
  | Version -> print_version ()
  | Compile { paths; flags } -> compile paths flags None
  | Error { msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg parse.prog |> print_error
