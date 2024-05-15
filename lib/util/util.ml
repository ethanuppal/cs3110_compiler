let ( >> ) f g x = g (f x)
let read_file filename = BatFile.with_file_in filename BatIO.read_all

let write_file filename content =
  BatFile.with_file_out filename (fun oc -> BatIO.write_line oc content)

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

let contains_substring str sub =
  let re = Str.regexp_string sub in
  try
    ignore (Str.search_forward re str 0);
    true
  with Not_found -> false

let uncurry f (x, y) = f x y

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

let pp_of string_of fmt x = Format.fprintf fmt "%s" (string_of x)
