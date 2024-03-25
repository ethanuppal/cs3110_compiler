module StringHashTable = Hashtbl.Make (String)

type t = Value.t StringHashTable.t

let empty () = StringHashTable.create 16
let store = StringHashTable.add
let lookup name scope = StringHashTable.find_opt scope name
let bindings = StringHashTable.to_seq
