(** An IR simulator. *)
type t

(** [make ()] is a new IR simulator. *)
val make : unit -> t

(** [run simulator cfg] simulates [cfg] using [simulator]. *)
val run : t -> Cfg.t -> unit

(** [dump simulator] is the current standard output of [simulator] as a
    human-readable string. *)
val output_of : t -> string
