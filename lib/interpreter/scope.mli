type t

val store : t -> string -> Value.t -> unit
val lookup : string -> t -> Value.t option
val bindings : t -> (string * Value.t) Seq.t
