type flag =
  | OnlyIR
  | OnlyObject
  | Optimize

type action =
  | Error of { msg : string }
  | Help
  | Version
  | Compile of {
      paths : string list;
      flags : flag list;
    }

type parse_result = {
  prog : string;
  action : action;
}

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
