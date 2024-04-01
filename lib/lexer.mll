{
  open Parser
}

rule read = parse
| eof { EOF }
| [' ' '\t' '\r'] { read lexbuf }
| '\n' { NEWLINE }
| "//" [^ '\n']* '\n' { NEWLINE }
| '=' { ASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| "let" { LET }
| "print" { PRINT }
| "func" { FUNC }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* { VAR (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { CONST (int_of_string (Lexing.lexeme lexbuf)) }
