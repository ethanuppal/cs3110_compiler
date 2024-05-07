(** Compiler flags *)
type flag =
  | OnlyIR
  | Optimize

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
      flags : flag list;
    }

(** [parse args] is the command line [args] parsed. *)
let parse : string array -> t =
  let open Util in
  let parse_aux = function
    | [ prog; "-h" ] | [ prog; "--help" ] -> Help { prog }
    | [ prog; "-v" ] | [ prog; "--version" ] -> Version { prog }
    | prog :: path :: rest ->
        let flags =
          rest
          |> List.filter_map (fun s ->
                 match s with
                 | "-g" | "--gen" -> Some OnlyIR
                 | "-O" | "--optimize" -> Some Optimize
                 | _ -> None)
        in
        File { prog; path; flags }
    | prog :: _ -> Error { prog; msg = "invalid arguments" }
    | _ -> failwith "program invoked with empty argument array"
  in
  Array.to_list >> parse_aux
