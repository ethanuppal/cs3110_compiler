(** A scope in which bindings can be made. *)
type t

(** [empty ()] creates an empty scope. *)
val empty : unit -> t

(** [store scope name value] adds the binding [name = value] to [scope]. *)
val store : t -> string -> Value.t -> unit

(** [lookup scope name] is the [value] bound to [name] in [scope], or [None] if
    [name] is not bound. *)
val lookup : t -> string -> Value.t option

(** [bindings scope] is the set of bindings in [scope]. *)
val bindings : t -> (string * Value.t) Seq.t
