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

let compile paths flags build_dir_loc =
  let do_opts = List.mem Cli.Optimize flags in
  let compile_one source_path =
    let source = Util.read_file source_path in
    let statements = Parse_lex.lex_and_parse ~filename:source_path source in
    Analysis.infer statements;
    let cfgs = Ir_gen.generate statements in
    let text_section = Asm.Section.make "text" 16 in
    Asm_emit.emit_preamble ~text:text_section;
    List.iter
      (fun cfg ->
        let liveliness_analysis = Liveliness.analysis_of cfg in
        if do_opts then
          Passes.apply
            [
              Passes.DeadCode.pass;
              Pass.sequence Passes.CopyProp.pass Passes.ConstFold.pass
              |> Pass.repeat 10;
            ]
            cfg liveliness_analysis;
        let instr_ordering = InstrOrdering.make cfg in
        let registers = Register.data_registers in
        let regalloc =
          Regalloc.allocate_for cfg registers liveliness_analysis instr_ordering
        in
        Asm_emit.emit_cfg ~text:text_section cfg regalloc)
      cfgs;
    let asm_file = Asm.AssemblyFile.make () in
    Asm.AssemblyFile.add asm_file text_section;
    let file_name_root =
      BatFilename.(source_path |> basename |> chop_extension)
    in
    let ir_file_name = file_name_root ^ ".x86istmb_ir" in
    let asm_file_name = file_name_root ^ ".nasm" in
    (ir_file_name, cfgs, asm_file_name, asm_file)
  in

  Printf.printf "[DEBUG] ignores some flags but -O works\n";

  try
    let compiled_files = List.map compile_one paths in

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

    let platform = Platform.get_platform () in

    (* Write NASM *)
    List.iter
      (fun (ir_file_name, cfgs, asm_file_name, asm_file) ->
        Util.write_file ir_file_name
          (cfgs |> List.map Cfg.to_string |> String.concat "\n");
        Util.write_file asm_file_name (Asm.AssemblyFile.to_nasm asm_file))
      compiled_files;

    (* Run NASM *)
    let object_format =
      match Platform.object_format platform with
      | Some format -> format
      | None -> failwith "Could not determine object file format."
    in
    List.iter
      (fun (_, _, asm_file_name, _) ->
        let nasm_command =
          Printf.sprintf "nasm -f %s %s -o %s.o" object_format asm_file_name
            (BatFilename.chop_extension asm_file_name)
        in
        if Sys.command nasm_command <> 0 then failwith "Failed to run NASM.")
      compiled_files;

    (* Run clang *)
    let runtime_folder_name =
      match platform.os with
      | Linux -> "linux"
      | MacOS _ -> "macos"
      | _ -> failwith "OS unknown. Cannot determine correct runtime."
    in
    let clang_target =
      match Platform.clang_target platform with
      | Some target -> target
      | None -> failwith "Unable to determine correct clang target."
    in
    let runtime_lib_loc =
      Util.merge_paths [ Project_root.path; "lib/runtime"; runtime_folder_name ]
    in
    let clang_command =
      Printf.sprintf "clang -target %s *.o %s/* -o a.out" clang_target
        runtime_lib_loc
    in
    if Sys.command clang_command <> 0 then failwith "Failed to run clang.";

    Printf.printf "==> Wrote build files to %s\n" build_dir;
    Printf.printf
      "==> You can run the executable with %s, prefixing with Rosetta as \
       appropriate\n"
      (Util.merge_paths [ build_dir; "a.out" ])
  with Parse_lex.ParserError msg -> print_error (msg ^ "\n")

let rec dispatch action prog =
  match action with
  | Cli.Help -> print_help prog
  | Version -> print_version ()
  | Compile { paths; flags } ->
      if List.is_empty paths then
        dispatch (Error { msg = "expected at least one file name" }) prog
      else compile paths flags None
  | Error { msg } -> Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error

let main args =
  let parse = Cli.parse args in
  dispatch parse.action parse.prog
