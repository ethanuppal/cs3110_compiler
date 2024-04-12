module Version : sig
  type t

  val make : int -> int -> int -> t
  val to_string : t -> string
end = struct
  type t = int * int * int

  let make maj min pat = (maj, min, pat)
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
    version = Version.make 0 1 0;
    description = "CS 3110 final project";
    authors =
      [
        "Utku Melemeti";
        "Jason Klein";
        "Jeffrey Huang";
        "Vijay Shanmugam";
        "Ethan Uppal";
      ];
  }
