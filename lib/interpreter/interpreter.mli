(** The type of input the interpreter should optimize for. *)
type interpreter_mode =
  | File
  | REPL
  | Text of string ref

(** The public interface of the interpreter. *)
type t = {
  dump : unit -> unit;
  step : Frontend.Ast.stmt -> unit;
  set_mode : interpreter_mode -> unit;
}

(** [create ()] creates a new interpreter instance. *)
val create : unit -> t
