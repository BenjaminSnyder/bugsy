open Ast

type sexpr = typ * sx
and sx =
    SNumLit of string
  | SStrLit of string
  | SBoolLit of bool
  | SArrayLiteral of sexpr list * typ
  | SIntLiteral of int
  | SId of string
  | SAccess of string * string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SConstruct of string * sexpr list
  | ArrayAssign of string * sexpr * sexpr
  | ArrayAccess of string * sexpr
  | SCrementop of sexpr * op
  | SCall of string * sexpr list
  | SArrayAccess of string * sexpr * typ
  | SArrayAssign of string * sexpr * sexpr
  | SClassCall of string * string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sconstruct_decl = {
    sctformals : bind list;
    sctlocals : bind list;
    sctbody : sstmt list;
  }

type sfunc_decl = {
    mutable styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sfbody : sstmt list;
  }

type scdecl = {
    scname : string;
    scdvars : bind list;
    scdconst: sconstruct_decl list;
    scdfuncs: sfunc_decl list;
    }

type sprogram = bind list * sfunc_decl list * scdecl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SNumLit(l) -> l
  | SIntLiteral(l) -> string_of_int l
  | SStrLit(l) -> l
  | SId(s) -> s
  | SAccess(s1,s2) -> s1 ^ "." ^ s2
  | SCrementop(e, o) -> string_of_sexpr e ^ " " ^ string_of_op o
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SConstruct(a, e) -> " new " ^ a ^ "(" ^ String.concat ", " (List.map string_of_sexpr e) ^ ")"
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SClassCall(c, f, el) ->
      c ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArrayLiteral(el, t) -> string_of_typ t ^ "[" ^ String.concat ", " (List.map (fun e -> string_of_sexpr e) el) ^ "]"
  | SArrayAccess(a, e, t) -> string_of_typ t ^ " " ^ a ^ "[" ^ string_of_sexpr e ^ "]"
  | SArrayAssign(a, left, right) -> a ^ "[" ^ string_of_sexpr left ^ "] = " ^ string_of_sexpr right

  | SNoexpr -> ""
  | _ -> raise (Failure "something was not implemented to print...")
				  ) ^ ")"

let rec add_slevel (listy, level) = match listy with
  [] -> []
  | hd::li' -> (hd,level):: add_slevel (li', level)

let rec string_of_sstmt (stmt,level) = match stmt with
    SBlock(stmts) ->
      "{\n" ^ (String.make level '\t') ^ String.concat (String.make level '\t') (List.map string_of_sstmt (add_level (stmts, level))) ^ (String.make (level-1) '\t') ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")" ^ string_of_sstmt (s, level)
  | SIf(e, s1, s2) -> "if (" ^ string_of_sexpr e ^ ")" ^
      string_of_sstmt (s1, (level)) ^ "else" ^ string_of_sstmt (s2, (level))
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt (s, (level+1))
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt (s, (level+1))

let string_of_svdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"


let string_of_sconst_decl sconst_decl =
  "constructor(" ^ String.concat ", " (List.map snd sconst_decl.sctformals) ^
  ") {\n\t\t" ^
  String.concat "\t\t" (List.map string_of_svdecl sconst_decl.sctlocals) ^ "\t\t" ^
  String.concat "\t\t" (List.map string_of_sstmt (add_level (sconst_decl.sctbody, 1))) ^ "\t" ^
  "}\n"

let string_of_sfdecl (sfdecl, level) =
  string_of_typ sfdecl.styp ^ " " ^
  sfdecl.sfname ^ "(" ^ String.concat ", " (List.map snd sfdecl.sformals) ^
  ") {\n" ^ (String.make (level) '\t') ^
  String.concat (String.make level '\t') (List.map string_of_svdecl sfdecl.slocals) ^ (String.make level '\t') ^
  String.concat (String.make level '\t') (List.map string_of_sstmt (add_level (sfdecl.sfbody, (level+1)))) ^ (String.make (if level-1 < 0 then 0 else level-1) '\t') ^
  "}\n"

let string_of_scdecl (scdecl, level) =
  "class " ^ scdecl.scname ^ " {" ^ "\n" ^ if (List.length scdecl.scdvars) < 0 then "" else "\t" ^
  String.concat "\t" (List.map string_of_svdecl scdecl.scdvars) ^ "\t" ^
  String.concat "\t" (List.map string_of_sconst_decl scdecl.scdconst) ^ "\t" ^
  String.concat "\t" (List.map string_of_sfdecl (add_level (scdecl.scdfuncs, (level+1)))) ^ (String.make (level-1) '\t') ^
  "}\n"

let string_of_sprogram (svars, sfuncs, sclasses) =
  String.concat "" (List.map string_of_svdecl svars) ^ "\n" ^
  String.concat "" (List.map string_of_scdecl (add_level (sclasses, 1))) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl (add_level (sfuncs, 1)))

