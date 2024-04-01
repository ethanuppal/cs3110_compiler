exception ParseError of string

(** [lex_and_parse input] is the list of statements represented by the source
    code string [input].contents

    @raise ParseError on parsing error. *)
let lex_and_parse input =
  let lexbuf = Lexing.from_string input in
  try Parser.main Lexer.read lexbuf
  with Parser.Error -> raise (ParseError "unknown parser error")

(* https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)
(* https://stackoverflow.com/questions/38505920/get-the-input-string-that-raises-parsing-error-inside-the-parser *)
