(** An [id] is a unique integer. It is guaranteed that managing identifiers
    through the [Gen] module will yield unique identifiers across program
    execution. *)
type id = int

module Gen : sig
  (** Values of type [t] are unique-identifier generators. *)
  type t

  (** [make ()] is a new identifier generator. *)
  val make : unit -> t

  (** [id_of gen] is a unique identifier for [gen]. *)
  val id_of : t -> id

  (** [next gen] returns a unique identifier among all identifiers yielded by
      [gen]. In particular, the identifier returned is the integer successor of
      the value of the last call to this function. *)
  val next : t -> id
end = struct
  type t = {
    next : unit -> id;
    self_id : id;
  }

  let id_of gen = gen.self_id

  let make_aux self_id () =
    let id = ref 0 in
    {
      next =
        (fun () ->
          let result = !id in
          id := !id + 1;
          result);
      self_id;
    }

  let global = make_aux 0 ()
  let make () = make_aux (global.next ()) ()
  let next gen = gen.next ()
end
