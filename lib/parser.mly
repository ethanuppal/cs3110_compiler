%{
  open Ast
%}

%token <int> CONST
%token <string> VAR
%token PLUS MINUS TIMES PRINT ASSIGN LET
%token EOF
%token NEWLINE

%start <Ast.main> main
%type <Ast.expr> expr
%type <Ast.stmt> stmt
%type <Ast.prog> prog

%%

prog:
  separated_list(stmt, NEWLINE) EOF { $1 }

expr:
  | CONST { Const $1 }
  | VAR { Var $1 }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3} }

stmt:
  | PRINT expr NEWLINE { Print $2 }
  | LET VAR ASSIGN expr NEWLINE { Declaration ($2, $4) }