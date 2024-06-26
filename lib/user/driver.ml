type flag =
  | OnlyIR
  | Optimize

let compile paths flags build_dir_loc =
  List.iter
    (fun path ->
      if
        (not (String.ends_with path ~suffix:".x86istmb"))
        && not (String.ends_with path ~suffix:".x")
      then failwith "please use .x or .x86istmb file extensions")
    paths;
  let do_opts = List.mem Optimize flags in
  let preamble_source =
    Util.read_file
      (Util.merge_paths [ Project_root.path; "lib/runtime/preamble.x" ])
  in
  let preamble_statements =
    ParseLex.lex_and_parse ~filename:"preamble.x" preamble_source
  in
  let compile_one preamble_statements source_path =
    Printf.printf "==> \x1B[32mCompiling \x1B[4m%s\x1B[m\n" source_path;
    let source = Util.read_file source_path in
    let statements =
      preamble_statements @ ParseLex.lex_and_parse ~filename:source_path source
    in
    Analysis.infer statements;
    let cfgs, ffi_names, decl_names = IrGen.generate statements in
    let text_section = Asm.Section.make "text" 16 in
    let data_section = Asm.Section.make "data" 16 in
    AsmEmit.emit_preamble ~text_section ~data_section ffi_names decl_names;
    List.iter
      (fun cfg ->
        let liveliness_analysis = Liveliness.analysis_of cfg in
        if do_opts then
          Passes.apply
            [
              Pass.sequence Passes.CopyProp.pass Passes.ConstFold.pass
              |> Pass.repeat 10;
            ]
            cfg liveliness_analysis;
        let instr_ordering = InstrOrdering.make cfg in

        (* Don't let the allocator use parameter registers, we'll need those in
           emission. *)
        let registers =
          List.filter
            (fun reg -> not (List.mem reg Asm.Register.parameter_registers))
            Asm.Register.data_registers
        in
        let regalloc =
          Regalloc.allocate_for cfg registers liveliness_analysis instr_ordering
        in
        AsmEmit.emit_cfg ~text_section ~data_section cfg regalloc)
      cfgs;
    if do_opts then AsmClean.clean text_section;
    let asm_file = Asm.AssemblyFile.make () in
    Asm.AssemblyFile.add asm_file text_section;
    Asm.AssemblyFile.add asm_file data_section;
    let file_name_root =
      BatFilename.(source_path |> basename |> chop_extension)
    in
    let ir_file_name = file_name_root ^ ".x86istmb_ir" in
    let asm_file_name = file_name_root ^ ".nasm" in
    (ir_file_name, cfgs, asm_file_name, asm_file)
  in

  Printf.printf "\x1B[2m[DEBUG] ignores some flags but -O works\x1B[m\n";

  let compiled_files =
    compile_one []
      (Util.merge_paths [ Project_root.path; "lib/runtime/linkonce.x" ])
    :: List.map (compile_one preamble_statements) paths
  in

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

  (* Write IR *)
  List.iter
    (fun (ir_file_name, cfgs, _, _) ->
      Util.write_file ir_file_name
        (cfgs |> List.map Cfg.to_string |> String.concat "\n\n"))
    compiled_files;

  if not (List.mem OnlyIR flags) then (
    (* Write NASM *)
    List.iter
      (fun (_, _, asm_file_name, asm_file) ->
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
        if Sys.command nasm_command <> 0 then failwith "Failed to run NASM."
        else
          Printf.printf "==> \x1B[32mGenerated \x1B[4m%s/%s\x1B[m\n" build_dir
            asm_file_name)
      compiled_files;

    (* Run clang *)
    let runtime_lib_loc =
      Util.merge_paths [ Project_root.path; "lib/runtime/src" ]
    in

    let nasm_command =
      Printf.sprintf "nasm -f %s %s -o %s" object_format
        (Util.merge_paths [ Project_root.path; "lib/runtime/integrity.nasm" ])
        "_integrity.o"
    in
    if Sys.command nasm_command <> 0 then
      failwith
        ("Failed to run NASM to handle integrity.nasm runtime: " ^ nasm_command);

    let clang_target =
      match Platform.clang_target platform with
      | Some target -> target
      | None -> failwith "Unable to determine correct clang target."
    in

    let define =
      match platform.os with
      | MacOS _ -> "X86ISTMB_MACOS"
      | Linux -> "X86ISTMB_LINUX"
      | Unknown -> failwith "Unable to determine correct runtime define."
    in

    let clang_command =
      Printf.sprintf "clang -D%s -target %s *.o %s/* -o a.out" define
        clang_target runtime_lib_loc
    in
    if Sys.command clang_command <> 0 then failwith "Failed to run clang.");

  Printf.printf "==> \x1B[32mWrote build files to \x1B[4m%s\x1B[m\n" build_dir;

  if not (List.mem OnlyIR flags) then
    Printf.printf
      "==> \x1B[33mYou can run the executable with \x1B[3m%s%s\x1B[m\n"
      (Platform.command_prefix platform)
      (Util.merge_paths [ build_dir; "a.out" ])
