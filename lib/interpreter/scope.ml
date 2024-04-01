module StringHashTable = Hashtbl.Make (String)

type t = Value.t StringHashTable.t

let empty () = StringHashTable.create 16
let store = StringHashTable.add
let lookup = StringHashTable.find_opt
let bindings = StringHashTable.to_seq
