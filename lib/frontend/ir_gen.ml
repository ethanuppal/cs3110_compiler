open Ast

let gen_func (Function { body }) =
  ignore body;
  []

let generate prog = List.map gen_func prog
