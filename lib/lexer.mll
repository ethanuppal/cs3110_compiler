{
  open Parser
}

rule read = parse
  | [' ' '\t' '\r'] { read lexbuf }
  | "=" { ASSIGN }
  | "let" { LET }
  | "print" { PRINT }
  | ['0'-'9']+ { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z' 'A'-'Z' '_']+ { VAR (Lexing.lexeme lexbuf) }
  | "\n" { NEWLINE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  (* | eof { EOF } *)
  | "$" { DOLLAR }