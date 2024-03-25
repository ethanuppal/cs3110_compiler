(** [f >> g] is the function that first evaluates [f] on its input and then [g]
    on the output of [f]. *)
let ( >> ) f g x = g (f x)
