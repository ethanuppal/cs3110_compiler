(** The type ['a t] is a stack-based context binding strings to ['a]. *)
type 'a t

(** [make ()] is a new context. *)
val make : unit -> 'a t

(** [stack_size ctx] is the number of scopes in [ctx]. *)
val stack_size : 'a t -> int

(** [is_empty ctx] if and only if [ctx] has no scopes. *)
val is_empty : 'a t -> bool

(** [push ctx] pushes a scope onto the stack. *)
val push : 'a t -> unit

(** [pop ctx] pops a scope off the top of the stack.

    Requires: [not (is_empty ctx)]. *)
val pop : 'a t -> unit

(** [insert ctx key value] inserts the association [(key, value)] into the top
    scope in [ctx].

    Requires: [not (is_empty ctx)]. *)
val insert : 'a t -> string -> 'a -> unit

(** [get ctx key] is the value associated with [key] in the scope closest to the
    top of the stack in which [key] appears, or [None] if [key] is not bound. *)
val get : 'a t -> string -> 'a option

(** [get_local ctx key] is the value associated with [key] in the top scope in
    [ctx], or [None] of [key] is not bound. *)
val get_local : 'a t -> string -> 'a option

(** [to_list ctx] is [ctx] as a list of key-value pair lists, where each list is
    a scope. Scopes that were pushed later are earlier in the result. *)
val to_list : 'a t -> (string * 'a) list list

val add_namespace : 'a t -> string -> unit
val pop_namespace : 'a t -> unit
val in_namespace : 'a t -> string -> string list
