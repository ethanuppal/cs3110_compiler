(** Compiler flags *)
type flag =
  | OnlyIR
  | OnlyObject
  | Optimize

(** The various parses of CLI arguments. *)
type action =
  | Error of { msg : string }
  | Help
  | Version
  | Compile of {
      paths : string list;
      flags : flag list;
    }

type t = {
  prog : string;
  action : action;
}

(** [parse args] is the command line [args] parsed. *)
let parse args =
  let parse_aux = function
    | [ "-h" ] | [ "--help" ] -> Help
    | [ "-v" ] | [ "--version" ] -> Version
    | args ->
        let paths = ref [] in
        let flags = ref [] in
        List.iter
          (fun s ->
            match s with
            | "-g" | "--gen" -> flags := OnlyIR :: !flags
            | "-O" | "--optimize" -> flags := Optimize :: !flags
            | "-c" | "--compile" -> flags := OnlyObject :: !flags
            | _ -> paths := s :: !paths)
          args;
        Compile { paths = !paths; flags = !flags }
  in
  {
    prog = Array.get args 0;
    action = args |> Array.to_list |> List.tl |> parse_aux;
  }
