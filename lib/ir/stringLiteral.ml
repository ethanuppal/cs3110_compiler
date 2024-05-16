type t = string * Id.t

let id_gen = Id.Gen.make ()
let make value = (value, Id.Gen.next id_gen)
let value_of (value, _) = value

let label_for (_, id) =
  "_x86istmb_Lstring_literal" ^ (id |> Id.int_of |> string_of_int)

(* todo: handle escapes *)
let to_string (value, _) = "\"" ^ value ^ "\""
