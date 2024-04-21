(** An intermediate representation simulator. *)
type t

val make : unit -> t
val simulate : t -> Cfg.t -> unit
val dump : t -> string
val output_of : t -> string
