%{
open AstType
%}

%token <int> INT_LIT
%token <string> STRING_LIT
%token CONST_TRUE CONST_FALSE
%token <string> IDEN
%token PLUS MINUS TIMES DIVIDE MOD EQUALS BITAND
%token LPAR RPAR LBRACE RBRACE COLON ARROW COMMA SCOPE
%token PRINT ASSIGN LET FUNC IF ELSE WHILE RETURN NAMESPACE FFI DECL
%token NEWLINE EOF
%token INT_TYPE BOOL_TYPE

%left PLUS MINUS
%left TIMES

%start <AstType.stmt list> main

%type <AstType.stmt> stmt
%type <AstType.expr> expr
%type <Type.t> ty 

%%

main:
  | EOF { [] }
  | NEWLINE main { $2 }
  | stmt NEWLINE main { $1 :: $3 }

ty:
  | INT_TYPE { Type.Prim Int }
  | BOOL_TYPE { Type.Prim Bool }
  | ty TIMES { Type.Pointer ($1) }
  | IDEN { Type.Var $1 }

expr:
  | LPAR expr RPAR { $2 }
  | INT_LIT { ConstInt $1 }
  | CONST_TRUE { ConstBool true }
  | CONST_FALSE { ConstBool false }
  | IDEN { Var {name = $1; ty = None} }
  | STRING_LIT { StringLiteral { value = $1; ty = Some (Type.Pointer (Type.char_prim_type)) } }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3; ty = None} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3; ty = None} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3; ty = None} }
  | expr DIVIDE expr { Infix {lhs = $1; op = Divide; rhs = $3; ty = None} }
  | expr MOD expr { Infix {lhs = $1; op = Mod; rhs = $3; ty = None} }
  | expr EQUALS expr { Infix {lhs = $1; op = Equals; rhs = $3; ty = None} }
  | BITAND expr { Prefix {op = BitAnd; rhs = $2; ty = None} }
  | PLUS expr { Prefix {op = Plus; rhs = $2; ty = None} }
  | MINUS expr { Prefix {op = Minus; rhs = $2; ty = None} }
  | TIMES expr { Prefix {op = Times; rhs = $2; ty = None} }
  | name = IDEN; LPAR; args = separated_list(COMMA, expr); RPAR { Call { name = [name]; args; ty = None }}
  | name = IDEN; SCOPE; rest = separated_list(SCOPE, IDEN); LPAR; args = separated_list(COMMA, expr); RPAR { Call { name = name :: rest; args; ty = None } }

body_till_rbrace:
  | NEWLINE body_till_rbrace { $2 } 
  | RBRACE {[]}
  | stmt RBRACE {[$1]}
  | stmt NEWLINE body_till_rbrace { $1 :: $3 }

param:
  | IDEN COLON ty { ($1, $3) }

return_type:
  | ARROW ty { $2 }

stmt:
  | IF expr LBRACE body_till_rbrace { If {cond = $2; body = $4 } }
  | LET IDEN COLON ty ASSIGN expr { Declaration {name = $2; hint = Some ($4); expr = $6} }
  | LET IDEN ASSIGN expr { Declaration {name = $2; hint = None; expr = $4} }
  | IDEN ASSIGN expr { Assignment ($1, $3) }
  | FUNC; name = IDEN; LPAR; params = separated_list(COMMA, param); RPAR; return_opt = option(return_type); LBRACE; body = body_till_rbrace { Function {name; params; return = if return_opt = None then Type.unit_prim_type else Option.get (return_opt); body} }
  | PRINT expr { Print $2 }
  | RETURN; return_opt = option(expr) { Return (return_opt) }
  | expr { ExprStatement $1 }
  | NAMESPACE; name = IDEN; LBRACE; contents = body_till_rbrace { Namespace { name; contents }}
  | FFI; name = IDEN; LPAR; params = separated_list(COMMA, ty); RPAR; return_opt = option(return_type) { ForeignFunction {name; params; return = if return_opt = None then Type.unit_prim_type else Option.get (return_opt); } }
  | DECL; name = IDEN; LPAR; params = separated_list(COMMA, ty); RPAR; return_opt = option(return_type) { DeclaredFunction {name; params; return = if return_opt = None then Type.unit_prim_type else Option.get (return_opt); } }
