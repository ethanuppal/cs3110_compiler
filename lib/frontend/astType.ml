(** An arithmetic operation. *)
type op =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Equals
  | BitAnd

(** An expression can be evaluated to a value. *)
type expr =
  | Var of {
      name : string;
      mutable ty : Type.t option;
    }
  | ConstInt of int
  | ConstBool of bool
  | Infix of {
      lhs : expr;
      op : op;
      rhs : expr;
      mutable ty : Type.t option;
    }
  | Prefix of {
      op : op;
      rhs : expr;
      mutable ty : Type.t option;
    }
  | Call of {
      name : string list;
      args : expr list;
      mutable ty : Type.t option;
    }

(** A statement can be executed. *)
and stmt =
  | If of {
      cond : expr;
      body : stmt list;
    }
  | ExprStatement of expr
  | Declaration of {
      name : string;
      hint : Type.t option;
      expr : expr;
    }
  | Assignment of string * expr
  | Function of {
      name : string;
      params : (string * Type.t) list;
      return : Type.t;
      body : stmt list;
    }
  | Print of expr
  | Return of expr option
  | Namespace of {
      name : string;
      contents : stmt list;
    }

(** A program is a series of statements. *)
type prog = stmt list
