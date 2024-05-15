(** Compiler flags *)
type flag =
  | OnlyIR
  | OnlyObject
  | Optimize

(** The various actions the program can take. *)
type action =
  | Error of { msg : string }
  | Help
  | Version
  | Compile of {
      paths : string list;
      flags : flag list;
    }

(** The result of parsing CLI arguments. [prog] is the name/path of the running
    executable. [action] is what output the user requested. *)
type parse_result = {
  prog : string;
  action : action;
}

(** [parse args] is the command line arguments [args], parsed. *)
val parse : string array -> parse_result
