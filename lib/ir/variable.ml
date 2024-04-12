open Util

type t = int

let make x = x
let to_string = string_of_int >> ( ^ ) "i"
