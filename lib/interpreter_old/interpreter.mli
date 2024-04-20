(** WARNING: THIS CODE (interperter.mli and interpreter.ml) IS DEPRECATED. *)

(** The type of input the interpreter should optimize for. *)
type interpreter_mode =
  | File
  | REPL
  | Text of string ref

(** The public interface of the interpreter. *)
type t = {
  dump : unit -> unit;
  step : Ast.stmt -> unit;
  set_mode : interpreter_mode -> unit;
}

(** [create ()] creates a new interpreter instance. *)
val create : unit -> t
