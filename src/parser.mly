/* Ocamlyacc parser for bugsy */

%{
open Ast
let fst' (fs,_,_)=fs
let snd' (_,sn,_)=sn
let trd  (_,_,tr)=tr
%}

/* Declaring the tokens that we used */
%token CONSTRUCTOR CLASS NEW
%token LPAREN RPAREN LBRACE RBRACE LSQBRACKET RSQBRACKET
%token SEMI COMMA DOT
%token PLUS MINUS MULT DIV ASSIGN MODULO
%token INCREMENT DECREMENT
%token PLUSEQ MINUSEQ MULTEQ DIVEQ
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token RETURN IF ELSE FOR WHILE
%token BOOL VOID STRING NUM
%token <bool> BLIT
%token <string> ID NUMLIT STRLIT CLASSID
%token EOF

/* Adding precedence and associativity for tokens */
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

/* The entry point for the whole a Bugsy file */
program:
  decls EOF { $1 }

/* Declarations made in a busgy file */
decls:
  /* nothing */ { [], [], [] }
  | decls vdecl { ($2 :: fst' $1), snd' $1, trd $1 }
  | decls fdecl { fst' $1, ($2 :: snd' $1), trd $1 }
  | decls cdecl { fst' $1, snd' $1, ( $2 :: trd $1) }

/* Class declarations */
cddecls:
  /* nothing */ { [], [], [] }
  | cddecls vdecl { ($2 :: fst' $1), snd' $1, trd $1 }
  | cddecls const_decl { fst' $1, ($2 :: snd' $1), trd $1 }
  | cddecls fdecl { fst' $1, snd' $1, ($2 :: trd $1) }

/* Attributes for a class including its name, constructor, attributes, and functions */
cdecl:
  CLASS ID LBRACE cddecls RBRACE
  { {
    cname = $2;
    cdvars = fst' $4;
    cdconst = snd' $4;
    cdfuncs = trd $4;
  } }

/* Constructor consists of formals, locally declared vars, and the body */
const_decl:
  CONSTRUCTOR LPAREN formals_opt RPAREN LBRACE body RBRACE
  { {
    ctformals = List.rev $3;
    ctlocals = List.rev (fst $6);
    ctbody = List.rev (snd $6);
  } }

/* Function declaration has a return type, name, parameters, and the function body */
fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE body RBRACE
  { {
    typ = $1;
	  fname = $2;
	  formals = List.rev $4;
	  locals = List.rev (fst $7);
    fbody = List.rev (snd $7);
  } }

/* Body of a function consists of variable declarations and then statements */
body:
    /* nothing */   { [], [] }
  | body vdecl { (($2 :: fst $1), snd $1) }
  | body stmt  { (fst $1, ($2 :: snd $1)) }

/* The formal arguments that a function takes */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

/* The Ocaml list of formal arguments */
formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

/* Built in types for Bugsy such as strings and nums */
typ:
    NUM      { Num  }
  | BOOL     { Bool }
  | VOID     { Void }
  | STRING   { String }
  | array_t  { $1 }
  | CLASSID  { Object({
                className = $1;
                instanceVars = StringMap.empty;
              })
             }

/* Bugsy array type */
array_t:
  typ LSQBRACKET expr RSQBRACKET { Array($1, $3) }

/* Declaring a variable in Bugsy example: num n; */
vdecl:
    typ ID SEMI { ($1, $2) }

/* List of statements */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* Various statments in the Bugsy language such as if statements, or while loops */
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

/* If the expression is optional */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

/* Expressions in the Bugsy language */
expr:
    ID               { Id($1) }
  | CLASSID DOT ID        { Access($1, $3) }
  | literal          { $1 }
  | bool_expr        { $1 }
  | arithmetic       { $1 }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | CLASSID DOT ID LPAREN actuals_opt RPAREN { ClassCall($1, $3, $5) }
  | NEW ID LPAREN actuals_opt RPAREN { Construct($2, $4) }
  | ID ASSIGN expr                   { Assign($1, $3) }
  | ID LSQBRACKET expr RSQBRACKET ASSIGN expr           { ArrayAssign($1, $3, $6) }
  | ID LSQBRACKET expr RSQBRACKET    { ArrayAccess($1, $3) }
  | LPAREN expr RPAREN               { $2 } /* allow parentheses in arithmetic */

/* Built in types like type num or string */
literal:
    NUMLIT           { NumLit($1)  }
  | STRLIT           { StrLit($1)  }
  | BLIT             { BoolLit($1) }
  | LSQBRACKET arr_contents RSQBRACKET { ArrayLit(List.rev $2) }

/* Generate a list of exprs that make up the array */
arr_contents:
    expr            { [$1] }
  | arr_contents COMMA expr { $3 :: $1 }

/* Logical expressions that returns boolean values */
bool_expr:
    expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | NOT expr         { Unop(Not, $2) }

/* Bugsy arithmetic operations on num types */
arithmetic:
    expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr MULT   expr { Binop($1, Mult,  $3) }
  | expr DIV    expr { Binop($1, Div,   $3) }
  | expr MODULO expr { Binop($1, Mod,   $3) }
  | expr PLUSEQ  expr   { Binop($1, Pluseq, $3) }
  | expr MINUSEQ expr   { Binop($1, Mineq, $3)  }
  | expr MULTEQ  expr   { Binop($1, Multeq, $3) }
  | expr DIVEQ   expr   { Binop($1, Diveq, $3)  }
  | expr INCREMENT   { Crementop($1, PostInc) }
  | INCREMENT expr   { Crementop($2, PreInc)  }
  | expr DECREMENT   { Crementop($1, PostDec)  }
  | DECREMENT expr   { Crementop($2, PreDec) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }

/* The actual arguments (values) that you pass into the function */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

/* The Ocaml list of the actual arguments passed into a function call */
actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
