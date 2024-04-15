{
  open Parser
}

rule read = parse
| eof { EOF }
| [' ' '\t' '\r'] { read lexbuf }
| '\n' { NEWLINE }
| "//" [^ '\n']* '\n' { NEWLINE }
| "==" { EQUALS }
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
| ':' { COLON }
| "->" { ARROW }
| "Int" { INT_TYPE }
| "Bool" { BOOL_TYPE }
| "let" { LET }
| "print" { PRINT }
| "func" { FUNC }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "true" { CONST_TRUE }
| "false" { CONST_FALSE }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* { VAR (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { CONST_INT (int_of_string (Lexing.lexeme lexbuf)) }
