(** The type of an IR variable. *)
type t

val make : int -> t

(** [to_string var] is [var] as a string. *)
val to_string : t -> string
