open Ast

type sexpr = typ * sx
and sx =
    SNumLit of string
  | SIntLiteral of int
  | SBoolLit of bool
  | SId of string
  | SCrementop of sexpr * op
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SStrLit of string
  | SArrayLiteral of sexpr list * typ
  | SArrayAccess of string * sexpr * typ
  | SArrayAssign of string * sexpr * sexpr
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
  | SCrementop(e, o) -> string_of_sexpr e ^ " " ^ string_of_op o
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArrayLiteral(el, t) -> string_of_typ t ^ "[" ^ String.concat ", " (List.map (fun e -> string_of_sexpr e) el) ^ "]"
  | SArrayAccess(a, e, t) -> string_of_typ t ^ " " ^ a ^ "[" ^ string_of_sexpr e ^ "]"
  | SArrayAssign(a, e1, e2) -> a ^ "[" ^ string_of_sexpr e1 ^ "] = " ^ string_of_sexpr e2

  | SNoexpr -> ""
				  ) ^ ")"

let rec add_slevel (listy, level) = match listy with
  [] -> []
  | hd::li' -> (hd,level):: add_slevel (li', level)

let rec string_of_sstmt (stmt,level) = match stmt with
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt (add_slevel (stmts, (level+1)))) ^ "}\n"
  | SExpr(expr) -> (String.make level '\t') ^
string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> (String.make level '\t') ^
"return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> (String.make level '\t') ^
"if (" ^ string_of_sexpr e ^ ")" ^ string_of_sstmt (s, (level+1))
  | SIf(e, s1, s2) ->  (String.make level '\t') ^
"if (" ^ string_of_sexpr e ^ ")" ^
      string_of_sstmt (s1, (level+1)) ^ "else" ^ string_of_sstmt (s2, (level+1))
  | SFor(e1, e2, e3, s) ->
      (String.make level '\t') ^
"for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt (s, (level+1))
  | SWhile(e, s) -> (String.make level '\t') ^
"while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt (s, (level+1))

let string_of_svdecl (t, id, level) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec add_stplevel (listy, level) = match listy with
  [] -> []
  | hd::li' -> (fst hd, snd hd, level):: add_stplevel (li', level)

let string_of_sconst_decl const_decl =
  "constructor(" ^ String.concat ", " (List.map snd const_decl.sctformals) ^
  ") {\n\t" ^
  String.concat "\t" (List.map string_of_svdecl (add_stplevel (const_decl.sctlocals, 1))) ^ "\t" ^
  String.concat "\t" (List.map string_of_sstmt (add_slevel (const_decl.sctbody, 1))) ^ "\t" ^
  "}\n"

let string_of_sfdecl (fdecl, level) =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ") {\n" ^ "\t" ^
  String.concat "\t" (List.map string_of_svdecl (add_stplevel (fdecl.slocals, level+1))) ^
  String.concat "\t" (List.map string_of_sstmt (add_slevel (fdecl.sfbody, (level+1)))) ^ (String.make level '\t') ^
  "}\n"

let string_of_scdecl (cdecl, level) =
  "class " ^ cdecl.scname ^ " {" ^ "\n" ^ "\t" ^
  String.concat "\t" (List.map string_of_svdecl (add_stplevel (cdecl.scdvars,level+1))) ^ "\t" ^
  String.concat "\t" (List.map string_of_sconst_decl cdecl.scdconst) ^ "\t" ^
  String.concat "\t" (List.map string_of_sfdecl (add_slevel (cdecl.scdfuncs, (level+1)))) ^ (String.make level '\t') ^
  "}\n"

let string_of_sprogram (svars, sfuncs, sclasses) =
  String.concat "" (List.map string_of_svdecl (add_stplevel (svars,0))) ^ "\n" ^
  String.concat "" (List.map string_of_scdecl (add_slevel (sclasses, 0))) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl (add_slevel (sfuncs, 0)))

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

