/* Ocamlyacc parser for bugsy */

%{
open Ast
%}

%token CONSTRUCTOR CLASS NULL 
%token CONTINUE BREAK TRY CATCH RAISE
%token LPAREN RPAREN LBRACE RBRACE LSQBRACKET RSQBRACKET
%token COLON SEMI COMMA QMARK
%token PLUS MINUS MULT DIV ASSIGN MODULO
%token INCREMENT DECREMENT
%token PLUSEQ MINUSEQ MULTEQ DIVEQ 
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELIF ELSE FOR WHILE 
%token NUM BOOL VOID STRING CHAR 
%token POINT SHAPE SQUARE RECT CIRCLE ELLIPSE TRIANGLE
%token POLYGON REGAGON CANVAS LINE SPLINE
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%right PLUSEQ MINUSEQ MULTEQ DIVEQ
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MULT DIV MODULO
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
    cdecl decls EOF { $1, $2 }
 |  decls EOF { Nil, $1 }

decls: (* HANS HALP *)
  let fst' li = match li with [fs,_,_]->fs in
  let snd' li = match li with [_,sn,_]->sn in
  let trd li = match li with [_,_,tr]->tr in
  /* nothing */ { [], [], [] }
 | decls vdecl { [$2 :: fst' $1], snd' $1, (trd $1)  }
 | decls fdecl { fst' $1, [$2 :: snd' $1], (trd $1)  }
 | decls const_decl { fst' $1, snd' $1, [$1 :: [$2 :: (trd $1)]] }

cdecl:
    CLASS ID LBRACE decls RBRACE
    { {
    cname = $2;
    decls = $4; } } (*match $4 with and put them into separate fields *)

const_decl:
  CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  { {
  ctformals = $3;
  ctlocals = List.rev $6;
  ctbody = List.rev $7; } }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    NUM { Num }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }
  | CHAR { Char }
  | arr { $1 }
  | obj { $1 }

obj:
  SHAPE LPAREN NUM RPAREN                            { Shape($3)            }
  | SQUARE LPAREN POINT COMMA NUM RPAREN             { Square($3, $5)       }
  | RECT LPAREN POINT COMMA NUM COMMA NUM RPAREN     { Rect($3, $5, $7)     }
  | TRIANGLE LPAREN POINT COMMA NUM COMMA NUM RPAREN { Triangle($3, $5, $7) }
  | CIRCLE LPAREN POINT COMMA NUM RPAREN             { Circle($3, $5)       }
  | ELLIPSE LPAREN POINT COMMA NUM COMMA NUM RPAREN  { Ellipse($3, $5, $7)  }
  | LINE LPAREN POINT COMMA POINT RPAREN             { Line($3, $5)         }
  | CANVAS LPAREN 
      LSQBRACKET POINT COMMA POINT COMMA POINT COMMA POINT COMMA RSQBRACKET
    COMMA NUM COMMA NUM RPARENT                      { Canvas($4, $6, $8, $10, $14, $16) }
  | POLYGON LPAREN NUM COMMA ARRAY RPAREN { Polygon($3, $5) }
  | REGAGON LPAREN NUM COMMA RPAREN { Regagon($3) }
  | SPLINE LPAREN NUM COMMA ARRAY RPAREN { Spline($3, $5) }


arr:
  typ LSQBRACKET LITERAL RSQBRACKET ID ASSIGN LBRACE expr RBRACE
  { Array($1, $3, $5, $8) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

bool_expr:
    TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | NOT expr         { Unop(Not, $2) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } (* HANS HELP BOOLEAN EXPR OR NO? *)

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | bool_expr        { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | bool_expr QMARK expr COLON expr { Ternop($1, $3, $5) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
