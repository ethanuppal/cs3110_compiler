type os =
  | MacOS
  | Linux
  | Unknown

type cpu_arch =
  | Arm
  | X86_64
  | Unknown

type platform = {
  os : os;
  cpu_arch : cpu_arch;
}

let get_platform () =
  let uname = Util.get_command_output "uname" in
  let machine = Util.get_command_output "uname -m" in
  {
    os =
      (if Util.contains_substring uname "Darwin" then MacOS
       else if Util.contains_substring uname "Linux" then Linux
       else Unknown);
    cpu_arch =
      (if Util.contains_substring machine "x86_64" then X86_64
       else if Util.contains_substring machine "arm" then Arm
       else Unknown);
  }

let command_prefix platform =
  if platform.os = MacOS && platform.cpu_arch = Arm then "arch -x86_64" else ""

let object_format platform =
  match platform.os with
  | Linux -> Some "elf64"
  | MacOS -> Some "macho64"
  | _ -> None
