(** [id x = x] for all [x]. *)
let id x = x

(** [f >> g] is the function that first evaluates [f] on its input and then [g]
    on the output of [f]. *)
let ( >> ) f g x = g (f x)

(** [read_file path] is the contents of the file at [path]. If the file does not
    exist, the behavior is undefined. *)
let read_file filename = BatFile.with_file_in filename BatIO.read_all

(** [write_file filename content] writes [content] to the file at [filename]. If
    the file already exists, it is overwritten. *)
let write_file filename content =
  BatFile.with_file_out filename (fun oc -> BatIO.write_line oc content)

(** [get_command_output command] is the standard output of running [command] in
    the shell. *)
let get_command_output command =
  let ic = Unix.open_process_in command in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (acc ^ line ^ "\n")
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines ""

(** [contains_substring str sub] if and only if [str] contains [sub]. *)
let contains_substring str sub =
  let re = Str.regexp_string sub in
  try
    ignore (Str.search_forward re str 0);
    true
  with Not_found -> false

(** [(uncurry f) (x, y) = f x y]. *)
let uncurry f (x, y) = f x y

(** [merge_paths [path1; path2; ...]] is the result of merging the path
    components [path1], [path2], and so on, which we let [path]. The function's
    output is specified by the following invariants in the general case (see
    further below):

    - If [path1] started with ['/'], so does [path].
    - For each pair of adjacent paths [path1], [path2] in the input list, they
      appear in the same relative position in [path] but with every ['/']
      removed from the end of [path1] and the start of [path2] and a single
      ['/'] inserted in their place.

    There are also the following exceptions:

    - [merge_paths [] = ""]
    - [merge_paths [""] = ""]
    - [merge_paths [""; ""; ...] = ""] *)
let merge_paths paths =
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
  if List.is_empty paths then ""
  else
    let first = List.hd paths in
    let first =
      if String.length first = 0 then first
      else if String.get first 0 = '/' then "/" ^ trim_slashes first
      else first
    in
    first :: (List.tl paths |> List.map trim_slashes)
    |> List.filter (( = ) "" >> not)
    |> String.concat "/"

(** [basename path] is largest suffix of [path] not containing the character
    ['/']. *)
let basename =
  let rec basename_aux = function
    | [] | '/' :: _ -> ""
    | head :: tail -> basename_aux tail ^ String.make 1 head
  in
  String.to_seq >> List.of_seq >> List.rev >> basename_aux

let rec zip_shortest lst1 lst2 =
  match (lst1, lst2) with
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip_shortest t1 t2
  | _, _ -> []

(** [pp_of string_of] is a pretty printer for a type with the string conversion
    function [string_of] that simply prints the result of [string_of] inline. *)
let pp_of string_of fmt x = Format.fprintf fmt "%s" (string_of x)

module IdMap = Hashtbl.Make (Id)
