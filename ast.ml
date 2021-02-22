(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Num | Bool | Void | String | Char 

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type construct_decl = {
  ctformals : bind list;
  ctlocals : bind list;
  ctbody : stmt list;
}

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type cdecl = {
  cname : string;
  decls : bind list * func_decl list * construct_decl;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Char -> "char"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_const_decl const_decl = 
  "constructor(" ^ String.concat ", " (List.map snd const_decl.ctformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl const_decl.ctlocals) ^
  String.concat "" (List.map string_of_stmt const_decl.ctbody) ^
  "}\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_cdecl cdecl = 
  cdecl.cname ^ "{" ^
  String.concat "" (List.map string_of_vdecl ((fun (fs,_,_)->fs) cdecl.decls)) ^
  String.concat "" (List.map string_of_stmt ((fun (_,sn,_)->sn) cdecl.decls)) ^
  String.concat "" (string_of_const_decl ((fun (_,_,tr)->tr) cdecl.decls)) ^
  "}\n"

let string_of_program (cdecl, (const, vars, funcs)) = function
  (Nil, (vars, funcs)) -> String.concat "" (List.map string_of_vdecl vars) ^
  "\n" ^ String.concat "\n" (List.map string_of_fdecl funcs)

  | (cdecl, (vars, funcs)) -> String.concat "" (string_of_cdecl cdecl) ^
  "\n" String.concat "" (string_of_const_decl const) ^ "\n" ^ String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
