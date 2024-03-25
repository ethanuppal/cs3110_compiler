{
  open Parser
}

rule read = parse
  | [' ' '\t' '\r'] { read lexbuf }
  | "=" { ASSIGN }
  | "let" { LET }
  | "print" { PRINT }
  | "func" { FUNC }
  | ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']* { VAR (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | "\n" { NEWLINE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "%" { MOD }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | eof { EOF }
