(** [f >> g] is the function that first evaluates [f] on its input and then [g]
    on the output of [f]. *)
let ( >> ) f g x = g (f x)

(** [read_file path] is the contents of the file at [path]. If the file does not
    exist, the behavior is undefined. *)
let read_file path =
  BatFile.lines_of path |> BatList.of_enum
  |> List.map (fun e -> e ^ "\n")
  |> String.concat ""

let uncurry f (x, y) = f x y

let merge_paths =
  let string_to_chars = String.to_seq >> List.of_seq in
  let trim_slashes str =
    let trim_prefix = function
      | '/' :: rest -> rest
      | chars -> chars
    in
    let chars = string_to_chars str in
    let trimmed_chars =
      chars |> trim_prefix |> List.rev |> trim_prefix
      |> List.rev_map (String.make 1)
    in
    String.concat "" trimmed_chars
  in
  List.map trim_slashes >> String.concat "/"
