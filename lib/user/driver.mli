type flag =
  | OnlyIR
  | Optimize

(** [compile paths flags build_dir_loc] *)
val compile : string list -> flag list -> string option -> unit
