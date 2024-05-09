type id = int
type t = id

let equal = Int.equal
let hash = Int.hash
let compare = Int.compare
let int_of id = id
let pp fmt id = Format.fprintf fmt "id[%i]" id

module Gen = struct
  type t = {
    mutable value : id;
    self_id : id;
  }

  let id_of gen = gen.self_id
  let make_aux start self_id () = { value = start; self_id }
  let global = make_aux 1 0 ()

  let next gen =
    let result = gen.value in
    gen.value <- gen.value + 1;
    result

  let make () = make_aux 0 (next global) ()
  let hard_reset () = global.value <- 0
end
