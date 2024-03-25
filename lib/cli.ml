(** An additional flag passed to the CLI after a file is specified. *)
type flag =
  | UseInterpreter
  | UseCompiler

(** The various parses of CLI arguments. *)
type t =
  | Error of {
      prog : string;
      msg : string;
    }
  | Help of { prog : string }
  | Version of { prog : string }
  | File of {
      prog : string;
      path : string;
      flag : flag;
    }
  | Repl of { prog : string }

(** [parse args] is the command line [args] parsed. *)
let parse : string array -> t =
  let open Util in
  let parse_aux = function
    | [ prog; "-h" ] | [ prog; "--help" ] -> Help { prog }
    | [ prog; "-v" ] | [ prog; "--version" ] -> Version { prog }
    | [ prog; "-r" ] | [ prog; "--repl" ] -> Repl { prog }
    | prog :: "-f" :: path :: rest | prog :: "--file" :: path :: rest -> (
        match rest with
        | [] | [ "-i" ] -> File { prog; path; flag = UseInterpreter }
        | [ "-c" ] -> File { prog; path; flag = UseCompiler }
        | _ -> Error { prog; msg = "expected -i or -c after file" })
    | prog :: _ -> Error { prog; msg = "invalid arguments" }
    | _ -> failwith "program invoked with empty argument array"
  in
  Array.to_list >> parse_aux
