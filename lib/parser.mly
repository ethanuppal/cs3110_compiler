%{
  open Ast
%}

%token <int> CONST
%token <string> VAR
%token PLUS MINUS TIMES 
%token PRINT ASSIGN LET
%token NEWLINE
%token DOLLAR

%start <Ast.stmt list> main

%type <Ast.stmt> stmt
%type <Ast.expr> expr

%%

main:
  | DOLLAR { [] }
  | stmt NEWLINE main { $1 :: $3 }

expr:
  | CONST { Const $1 }
  | VAR { Var $1 }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3} }

stmt:
  | LET VAR ASSIGN expr { Declaration ($2, $4) }
  | PRINT expr { Print $2 }