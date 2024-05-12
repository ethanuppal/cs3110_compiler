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

let compile paths _flags build_dir_loc =
  Printf.printf "[DEBUG] assumes [paths] has one file, ignores flags\n";
  let source_path = List.hd paths in
  let source = Util.read_file source_path in

  try
    (* Compile to NASM *)
    let statements = Parse_lex.lex_and_parse ~filename:source_path source in
    Analysis.infer statements;
    let cfgs = Ir_gen.generate statements in
    let text_section = Asm.Section.make "text" 16 in
    Asm_emit.emit_preamble ~text:text_section;
    List.iter
      (fun cfg ->
        let liveliness_analysis = Liveliness.analysis_of cfg in
        let instr_ordering = InstrOrdering.make cfg in
        let regalloc =
          Regalloc.allocate_for cfg liveliness_analysis instr_ordering
        in
        Asm_emit.emit_cfg ~text:text_section cfg regalloc)
      cfgs;
    let asm_file = Asm.AssemblyFile.make () in
    Asm.AssemblyFile.add asm_file text_section;

    (* Set up build directory *)
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
      failwith "Could not create folder build_dir/ in current directory.";
    if Sys.command (Printf.sprintf "rm -f %s/*" build_dir) <> 0 then
      failwith "Could not remove old build_dir/ contents.";
    Sys.chdir build_dir;

    (* Write NASM *)
    let asm_file_name =
      BatFilename.(source_path |> basename |> chop_extension) ^ ".nasm"
    in
    Util.write_file asm_file_name (Asm.AssemblyFile.to_nasm asm_file);

    let platform = Platform.get_platform () in

    (* Run NASM *)
    let object_format =
      match Platform.object_format platform with
      | Some format -> format
      | None -> failwith "Could not determine object file format."
    in
    let nasm_command =
      Printf.sprintf "nasm -f %s %s -o build.o" object_format asm_file_name
    in
    if Sys.command nasm_command <> 0 then failwith "Failed to run NASM.";

    (* Run clang *)
    let runtime_folder_name =
      match platform.os with
      | Linux -> "linux"
      | MacOS -> "macos"
      | _ -> failwith "OS unknown. Cannot determine correct runtime."
    in
    let runtime_lib_loc =
      Util.merge_paths [ Project_root.path; "lib/runtime"; runtime_folder_name ]
    in
    let clang_command =
      Printf.sprintf "clang -target x86_64 build.o %s/* -o a.out"
        runtime_lib_loc
    in
    if Sys.command clang_command <> 0 then failwith "Failed to run clang.";

    Printf.printf "==> Wrote build files to %s\n" build_dir;
    Printf.printf
      "==> You can run the executable with %s, prefixing with Rosetta as \
       appropriate\n"
      (Util.merge_paths [ build_dir; "a.out" ])
  with Parse_lex.ParserError msg -> print_error (msg ^ "\n")

let main args =
  let parse = Cli.parse args in
  match parse.action with
  | Help -> print_help parse.prog
  | Version -> print_version ()
  | Compile { paths; flags } -> compile paths flags None
  | Error { msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg parse.prog |> print_error
