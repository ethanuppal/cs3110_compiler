(** [main argv] *)
val main : string array -> unit

(** [compile paths flags build_dir_loc] *)
val compile : string list -> Cli.flag list -> string option -> unit
