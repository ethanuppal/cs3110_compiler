let lex_and_parse input =
  let lexbuf = Lexing.from_string input in
  try Parser.main Lexer.read lexbuf
  with Parser.Error -> failwith "Parser Error"
