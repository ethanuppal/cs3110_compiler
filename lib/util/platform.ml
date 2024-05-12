type os =
  | MacOS of int
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
  let os =
    if Util.contains_substring uname "Darwin" then
      let major_version =
        Util.get_command_output "sw_vers -productVersion"
        |> String.split_on_char '.' |> List.hd |> int_of_string_opt
      in
      match major_version with
      | Some major -> MacOS major
      | None -> Unknown
    else Linux
  in
  let cpu_arch =
    if Util.contains_substring machine "x86_64" then X86_64
    else if Util.contains_substring machine "arm" then Arm
    else Unknown
  in
  { os; cpu_arch }

let clang_target platform =
  match platform.os with
  | MacOS ver -> Some (Printf.sprintf "x86_64-apple-macos%i" ver)
  | Linux -> Some "x86_64"
  | Unknown -> None

let object_format platform =
  match platform.os with
  | Linux -> Some "elf64"
  | MacOS _ -> Some "macho64"
  | _ -> None
