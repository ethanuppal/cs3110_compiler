(** [id x = x] for all [x]. *)
let id x = x

(** [f >> g] is the function that first evaluates [f] on its input and then [g]
    on the output of [f]. *)
let ( >> ) f g x = g (f x)

(** [read_file path] is the contents of the file at [path]. If the file does not
    exist, the behavior is undefined. *)
let read_file filename = BatFile.with_file_in filename BatIO.read_all

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

(** [pp_of string_of] is a pretty printer for a type with the string conversion
    function [string_of] that simply prints the result of [string_of] inline. *)
let pp_of string_of fmt x = Format.fprintf fmt "%s" (string_of x)
