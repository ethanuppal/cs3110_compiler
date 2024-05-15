type analysis_error_info =
  | GeneralInfo
  | NameInfo of { symbol : string }
  | TypeInfo of
      [ `Mismatch of Type.t * Type.t
      | `InvalidSig of string * Type.t list
      | `DerefRValue of Type.t
      ]
  | HaltInfo of { name : string }

exception
  AnalyzerError of {
    info : analysis_error_info;
    msg : string option;
    ast : (AstType.expr, AstType.stmt) Either.t;
  }

(** After [infer prog], the contents of [prog] will have resolved types based on
    inference rules.

    @raise AnalyzerError on failure. *)
val infer : AstType.prog -> unit
