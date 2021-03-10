/* Ocamlyacc parser for bugsy */

%{
open Ast
let fst' = (fs,_,_)->fs
let snd' = (_,sn,_)->sn
let trd  = (_,_,tr)->tr
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
%token POLYGON REGAGON CANVAS LINE SPLINE ARRAY
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left PLUSEQ MINUSEQ
%left MULT DIV MODULO
%left MULTEQ DIVEQ
%right NOT NEG 
%nonassoc INCREMENT DECREMENT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { [], [], [] }
  | decls vdecl { ($2 :: fst' $1), snd' $1, trd $1 }
  | decls fdecl { fst' $1, ($2 :: snd' $1), trd $1 }
  | decls cdecl { fst' $1, snd' $1, ( $2 :: trd $1) }

cddecls: 
  /* nothing */ { [], [], [] }
  | cddecls vdecl { ($2 :: fst' $1), snd' $1, trd $1 }
  | cddecls const_decl { fst' $1, [$2], trd $1 }
  | cddecls fdecl { fst' $1, snd' $1, ($2 :: trd $1) }

cdecl:
  CLASS ID LBRACE cddecls RBRACE
  { {
    cname = $2;
    cdvars = fst' $4;
    cdconst = snd' $4;
    cdfuncs = trd $4;
  } }

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
  | obj { $1 } 
  | arr { $1 }

arr:
  typ LSQBRACKET NUM RSQBRACKET { Array }
  
obj:
  SHAPE      { () } 
  | SQUARE   { () }
  | RECT     { () }
  | TRIANGLE { () }
  | CIRCLE   { () }
  | ELLIPSE  { () }
  | LINE     { () }
  | CANVAS   { () }
  | POLYGON  { () }
  | REGAGON  { () }
  | SPLINE   { () }
  

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

li_contents:
    expr             {$1}
  | expr COMMA expr  {$3 :: $1}


expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | NOT expr         { Unop(Not, $2) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr MULT  expr { Binop($1, Mult,  $3) }
  | expr DIV expr { Binop($1, Div,   $3) }
  | expr MODULO expr { Binop($1, Mod, $3) }
  | ID PLUSEQ expr   { () }
  | ID MINUSEQ expr  { () }
  | ID MULTEQ expr   { () }
  | ID DIVEQ expr    { () }
  | expr INCREMENT   { () }
  | INCREMENT expr   { () }
  | expr DECREMENT   { () }
  | DECREMENT expr   { () }
  | LSQBRACKET li_contents RSQBRACKET { () }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } 
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
