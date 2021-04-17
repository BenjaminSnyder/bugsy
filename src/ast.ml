(* Abstract Syntax Tree and functions for printing it *)
module StringMap = Map.Make(String)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Mod | Pluseq | Mineq | Multeq | Diveq |
          PreInc | PostInc | PreDec | PostDec

type uop = Neg | Not

(* type arr = typ * literal *)


type expr =
    NumLit of string
  | StrLit of string
  | BoolLit of bool
  | ArrayLit of expr list
  | IntLiteral of int
  | Id of string
  | Access of string * string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Construct of string * expr list
  | ArrayAssign of string * expr * expr
  | ArrayAccess of string * expr
  | Crementop of expr * op
  | Call of string * expr list
  | ClassCall of string * string * expr list
  | Noexpr


type typ = Num | Bool | Void | Int |  String | Pt | Shape | Square | Rect |
           Triangle | Circle | Ellipse | Regagon | Polygon |
           Canvas | Line | Spline | Array of typ * expr | Object of classTyp

and bind = typ * string

and classTyp = {
  className : string;
  instanceVars : bind list;
}


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
    mutable typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    fbody : stmt list;
}

type cdecl = {
  cname : string;
  cdvars : bind list;
  cdconst: construct_decl list;
  cdfuncs: func_decl list;
}

type program = bind list * func_decl list * cdecl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=?"
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Pluseq -> "+="
  | Mineq -> "-="
  | Multeq -> "*="
  | Diveq -> "/="
  | PreInc | PostInc -> "++"
  | PreDec | PostDec -> "--"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    NumLit(nl)  -> nl
  | IntLiteral(l) -> string_of_int l
  | StrLit(str) -> "\"" ^ str ^ "\""
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(el) -> "[" ^ String.concat ", " (List.map (fun e -> string_of_expr e) el) ^ "]"
  | Id(s) -> s
  | Access(c, v) -> c ^ "." ^ v
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Crementop(e, o) -> (match o with
      PreInc -> string_of_op o ^ string_of_expr e
    | PostInc -> string_of_expr e ^ string_of_op o
    | PreDec -> string_of_op o ^ string_of_expr e
    | PostDec -> string_of_expr e ^ string_of_op o
    | _ -> "ERROR")

  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Construct(a, e) -> "new " ^ a ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | ArrayAccess(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | ArrayAssign(a, e1, e2) -> a ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ClassCall(c, f, el) -> 
      c ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec add_level (listy, level) = match listy with
  [] -> []
  | hd::li' -> (hd,level):: add_level (li', level)

let rec string_of_stmt (stmt,level) = match stmt with
    Block(stmts) ->
      "{\n" ^ (String.make level '\t') ^ String.concat (String.make level '\t') (List.map string_of_stmt (add_level (stmts, level))) ^ (String.make (level-1) '\t') ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")" ^ string_of_stmt (s, (level))
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")" ^
      string_of_stmt (s1, (level)) ^ (String.make (level-1) '\t') ^ "else" ^ string_of_stmt (s2, (level))
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt (s, (level+1))
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt (s, (level+1))

let rec string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Pt -> "point"
  | Shape -> "shape"
  | Square -> "square"
  | Rect -> "rect"
  | Triangle -> "triangle"
  | Circle -> "circle"
  | Ellipse -> "ellipse"
  | Regagon -> "regagon"
  | Polygon -> "polygon"
  | Canvas -> "canvas"
  | Line -> "line"
  | Spline -> "spline"
  | Array(t, e) -> string_of_typ t ^ "[" ^ string_of_expr e ^ "]"
  | Object(clas) -> clas.className
  | Int | _ -> raise ( Failure ("Not implemented in AST!"))

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_const_decl const_decl =
  "constructor(" ^ String.concat ", " (List.map snd const_decl.ctformals) ^
  ") {\n\t\t" ^
  String.concat "\t\t" (List.map string_of_vdecl const_decl.ctlocals) ^ "\t\t" ^
  String.concat "\t\t" (List.map string_of_stmt (add_level (const_decl.ctbody, 1))) ^ "\t" ^
  "}\n"

let string_of_fdecl (fdecl, level) =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ") {\n" ^ (String.make (level) '\t') ^
  String.concat (String.make level '\t') (List.map string_of_vdecl fdecl.locals) ^ (String.make level '\t') ^
  String.concat (String.make level '\t') (List.map string_of_stmt (add_level (fdecl.fbody, (level+1)))) ^ (String.make (if level-1 < 0 then 0 else level-1) '\t') ^
  "}\n"

let string_of_cdecl (cdecl, level) =
  "class " ^ cdecl.cname ^ " {" ^ "\n" ^ (if (List.length cdecl.cdvars) < 1 then "" else "\t") ^
  String.concat "\t" (List.map string_of_vdecl cdecl.cdvars) ^ (if (List.length cdecl.cdconst) < 1 then "" else "\t") ^
  String.concat "\t" (List.map string_of_const_decl cdecl.cdconst) ^ "\t" ^
  String.concat "\t" (List.map string_of_fdecl (add_level (cdecl.cdfuncs, (level+1)))) ^ (String.make (level-1) '\t') ^
  "}\n"

let string_of_program (vars, funcs, classes) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "" (List.map string_of_cdecl (add_level (classes, 1))) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (add_level (funcs, 1)))

