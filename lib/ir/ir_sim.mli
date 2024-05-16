(** An IR simulator. *)
type t

(** [make ()] is a new IR simulator. *)
val make : unit -> t

(** [run simulator cfgs] simulates [cfgs] using [simulator]. *)
val run : t -> Cfg.t list -> unit

(** [dump simulator] is the current standard output of [simulator] as a
    human-readable string. *)
val output_of : t -> string

(** [clear_output simulator] clears the output of the simulator. *)
val clear_output : t -> unit
