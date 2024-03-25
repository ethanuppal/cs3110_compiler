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
    }

let parse : string array -> t =
  let open X86ISTMB.Util in
  let parse_aux = function
    | [ prog; "-h" ] | [ prog; "--help" ] -> Help { prog }
    | [ prog; "-v" ] | [ prog; "--version" ] -> Version { prog }
    | [ prog; "-f"; path ] -> File { prog; path }
    | prog :: _ -> Error { prog; msg = "invalid arguments" }
    | _ -> failwith "program invoked with empty argument array"
  in
  Array.to_list >> parse_aux
