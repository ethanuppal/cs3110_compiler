%{
  open Ast
%}

%token <int> CONST_INT
%token CONST_TRUE CONST_FALSE
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE MOD 
%token LPAR RPAR LBRACE RBRACE COLON ARROW
%token PRINT ASSIGN LET FUNC IF ELSE WHILE
%token NEWLINE EOF
%token INT_TYPE BOOL_TYPE

%left PLUS MINUS
%left TIMES

%start <Ast.stmt list> main

%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Type.t> ty 

%%

main:
  | EOF { [] }
  | NEWLINE main { $2 }
  | stmt NEWLINE main { $1 :: $3 }

ty:
  | INT_TYPE { Type.Primitive Int63 }
  | BOOL_TYPE { Type.Primitive Bool }
  | ty TIMES { Type.Pointer ($1) }
  | VAR { Type.Var $1 }

expr:
  | LPAR expr RPAR { $2 }
  | CONST_INT { ConstInt $1 }
  | CONST_TRUE { ConstBool true }
  | CONST_FALSE { ConstBool false }
  | VAR { Var {name = $1; ty = None} }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3; ty = None} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3; ty = None} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3; ty = None} }
  | expr DIVIDE expr { Infix {lhs = $1; op = Divide; rhs = $3; ty = None} }
  | expr MOD expr { Infix {lhs = $1; op = Mod; rhs = $3; ty = None} }
  | PLUS expr { Prefix {op = Plus; rhs = $2; ty = None} }
  | MINUS expr { Prefix {op = Minus; rhs = $2; ty = None} }
  | TIMES expr { Prefix {op = Times; rhs = $2; ty = None} }

body_till_rbrace:
  | NEWLINE body_till_rbrace { $2 } 
  | RBRACE {[]}
  | stmt RBRACE {[$1]}
  | stmt NEWLINE body_till_rbrace { $1 :: $3 }

stmt:
  | VAR LPAR RPAR { Call $1 }
  | LET VAR COLON ty ASSIGN expr { Declaration {name = $2; hint = Some ($4); expr = $6} }
  | LET VAR ASSIGN expr { Declaration {name = $2; hint = None; expr = $4} }
  | VAR ASSIGN expr { Assignment ($1, $3) }
  | FUNC VAR LPAR RPAR LBRACE body_till_rbrace { Function {name = $2; body = $6} }
  | PRINT expr { Print $2 }
