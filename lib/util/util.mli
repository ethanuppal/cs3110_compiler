(** [f >> g] is the function that first evaluates [f] on its input and then [g]
    on the output of [f]. *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** [read_file path] is the contents of the file at [path]. If the file does not
    exist, the behavior is undefined. *)
val read_file : string -> string

(** [write_file filename content] writes [content] to the file at [filename]. If
    the file already exists, it is overwritten. *)
val write_file : string -> string -> unit

(** [get_command_output command] is the standard output of running [command] in
    the shell. *)
val get_command_output : string -> string

(** [contains_substring str sub] if and only if [str] contains [sub]. *)
val contains_substring : string -> string -> bool

(** [(uncurry f) (x, y) = f x y]. *)
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

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
val merge_paths : string list -> string

(** [basename path] is largest suffix of [path] not containing the character
    ['/']. *)
val basename : string -> string

(** [zip_shortest [a1; a2; ... an] [b1; b2; ... bm]] is
    [(a1, b1); (a2, b2); ... (ak, bk)] where [k = min(n, m)]. *)
val zip_shortest : 'a list -> 'b list -> ('a * 'b) list

(** [pp_of string_of] is a pretty printer for a type with the string conversion
    function [string_of] that simply prints the result of [string_of] inline. *)
val pp_of : ('a -> string) -> Format.formatter -> 'a -> unit
