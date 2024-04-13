exception
  TypeInferenceError of {
    domain : string;
    symbol : string option;
    ast : (Ast.expr, Ast.stmt) Either.t;
    unify : (Type.t * Type.t) option;
  }

(** After [infer prog], the contents of [prog] will have resolved types based on
    inference rules.

    @raise TypeInferenceError on failure. *)
val infer : Ast.prog -> unit
