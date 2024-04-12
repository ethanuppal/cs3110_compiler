type id = int

module Gen : sig
  type t

  val make : unit -> t
  val next : t -> id
end = struct
  type t = unit -> id

  let make () =
    let id = ref 0 in
    fun () ->
      let result = !id in
      id := !id + 1;
      result

  let next f = f ()
end
