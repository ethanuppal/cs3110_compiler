open Util

type t = int

let var_gen = Id.Gen.make ()
let make () = Id.Gen.next var_gen
let id_of var = var
let to_string = string_of_int >> ( ^ ) "i"
