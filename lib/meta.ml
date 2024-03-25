module Version = struct
  type t = int * int * int

  let to_string (maj, min, pat) = Printf.sprintf "v%d.%d.%d" maj min pat
end

type t = {
  name : string;
  version : Version.t;
  description : string;
  authors : string list;
}

let get : t =
  {
    name = "x86ISTMB";
    version = (0, 0, 0);
    description = "CS 3110 final project";
    authors =
      [
        "Utku Melemeti";
        "Jason Klein";
        "Utku Melemetci";
        "Jeffrey Huang";
        "Vijay Shanmugam";
        "Ethan Uppal";
      ];
  }
