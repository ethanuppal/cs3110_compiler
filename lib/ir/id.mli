(** An [id] is a unique identifier. It is guaranteed that managing identifiers
    through the [Gen] module will yield unique identifiers across program
    execution. *)
type id

(** See [id]. *)
type t = id

(** [int_of id] is an integer representation of [id], additionally guaranteed to
    be unique. *)
val int_of : id -> int

(** [equal id1 id2] if and only if [id1 = id2]. *)
val equal : id -> id -> bool

(** [compare id1 id2] is a comparison of [id1] and [id2] in accordance with
    comparison functions such as [Int.compare]. *)
val compare : id -> id -> int

(** [hash id] is a hash of [id]. *)
val hash : id -> int

val pp : Format.formatter -> id -> unit

module Gen : sig
  (** Values of type [t] are unique-identifier generators. *)
  type t

  (** [make ()] is a new identifier generator with the same starting seed as
      every other generator. *)
  val make : unit -> t

  (** [id_of gen] is a unique identifier for [gen]. *)
  val id_of : t -> id

  (** [next gen] returns a unique identifier among all identifiers yielded by
      [gen]. This identifier is determined exactly by the starting seed of the
      generator; see [make]. *)
  val next : t -> id

  val hard_reset : unit -> unit
end
