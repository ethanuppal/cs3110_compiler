type analysis_error_info =
  | GeneralInfo
  | NameInfo of { symbol : string }
  | TypeInfo of
      [ `Mismatch of Type.t * Type.t
      | `InvalidSig of string * Type.t list
      | `DerefRValue of Type.t
      ]

exception
  AnalyzerError of {
    info : analysis_error_info;
    msg : string option;
    ast : (Ast.expr, Ast.stmt) Either.t;
  }

(** After [infer prog], the contents of [prog] will have resolved types based on
    inference rules.

    @raise AnalyzerError on failure. *)
val infer : Ast.prog -> unit
