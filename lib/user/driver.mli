(** [main argv] is the entry point of the program. It takes in a string array
    [argv] and returns [unit]. The function parses the command line arguments
    using [Cli.parse], and then dispatches the appropriate action based on the
    parsed result. *)
val main : string array -> unit

(** [compile paths flags build_dir_loc] compiles the given list of paths into an
    executable.

    [paths] is a list of file paths to be compiled. [flags] is a list of
    compiler flags. [build_dir_loc] is an optional build directory location.

    Raises [Failure] if the file extensions are not .x or .x86istmb. *)
val compile : string list -> Cli.flag list -> string option -> unit
