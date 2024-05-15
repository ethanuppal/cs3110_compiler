(** Represents a type of operating system and, in the case of MacOS, a major
    version. *)
type os =
  | MacOS of int
  | Linux
  | Unknown

(** Represents a type of CPU architecture. *)
type cpu_arch =
  | Arm
  | X86_64
  | Unknown

(** A platform, consisting of an OS and cpu architecture. *)
type platform = {
  os : os;
  cpu_arch : cpu_arch;
}

(** [get_platform ()] is the platform for the system running this code. *)
val get_platform : unit -> platform

(** [clang_target platform] is the target clang should compile the runtime for
    on [platform]. *)
val clang_target : platform -> string option

(** [object_format platform] is the object file format that should be used on
    [platform]. *)
val object_format : platform -> string option
