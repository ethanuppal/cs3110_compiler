(** Module for versioning. *)
module Version : sig
  (** Represents a version of this program *)
  type t

  (** [make major minor patch] is the program version [major.minor.patch]. *)
  val make : int -> int -> int -> t

  (** [to_string version] is the string representation of [version]. *)
  val to_string : t -> string
end

(** Represents metadata about this program. *)
type t = {
  name : string;
  version : Version.t;
  description : string;
  authors : string list;
}

(** The metadata for this program. *)
val get : t
