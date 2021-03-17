type op =
    Add
  | Sub
  | Mult
  | Div
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
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
  cdvars : bind list;
  cdconst : construct_decl;
  cdfuncs : func_decl list;
}
type program = bind list * cdecl list * func_decl list
val string_of_op : op -> string
val string_of_uop : uop -> string
val string_of_expr : expr -> string
val string_of_stmt : stmt -> string
val string_of_typ : typ -> string
val string_of_vdecl : typ * string -> string
val string_of_const_decl : construct_decl -> string
val string_of_fdecl : func_decl -> string
val string_of_cdecl : cdecl -> string
val string_of_program :
  (typ * string) list * func_decl list * cdecl list -> string
