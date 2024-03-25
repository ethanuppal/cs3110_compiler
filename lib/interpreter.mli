type t = {
  dump : unit -> unit;
  step : Ast.stmt -> unit;
}

val create : unit -> t
