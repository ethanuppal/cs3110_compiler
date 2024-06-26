open Util

type t = Id.t

let var_gen = Id.Gen.make ()
let make () = Id.Gen.next var_gen
let id_of var = var
let to_string = Id.int_of >> string_of_int >> ( ^ ) "i"
let pp = pp_of to_string
let compare = Id.compare
let equal = Id.equal
let hash = Id.hash
