type t = {
  dump : unit -> unit;
  lookup : string -> Value.t option;
}

val create : unit -> t
