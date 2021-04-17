(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (globals, functions, classes) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)

  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty, "x")];
      locals = []; fbody = [] } map
    in let func_map = List.fold_left add_bind StringMap.empty [
			                         ("printb", Bool);
			                         ("print", Num);
                                     ("printf", String);
			                         ("printbig", Num);]

    in
    let add_bind2 map (name) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [];
      locals = []; fbody = [] } map
    in let func_map2 = List.fold_left add_bind2 func_map [
                                        ("demo");]
    in
(*    let add_bind3 map (name, ty1) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty1, "point")];
      locals = []; fbody = [] } map
    in let func_map3 = List.fold_left add_bind3 func_map2 [
                            ("add_point", Pt);]
    in *)
    let add_bind4 map (name, ty1, ty2) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty1, "x"); (ty2, "y")];
      locals = []; fbody = [] } map
    in let func_map4 = List.fold_left add_bind4 func_map2 [
                            ("add_point_xy", Num, Num);]
    in
    let add_bind5 map (name, ty1, ty2, ty3) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty1, "x"); (ty2, "y"); (ty3, "r")];
      locals = []; fbody = [] } map
    in let func_map5 = List.fold_left add_bind5 func_map4 [
                            ("add_circle", Num, Num, Num);]
    in
    let add_bind6 map (name, ty1, ty2, ty3) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty1, "x"); (ty2, "y"); (ty3, "size")];
      locals = []; fbody = [] } map
    in let func_map6 = List.fold_left add_bind6 func_map5 [
                            ("add_square", Num, Num, Num);]
    in
    let add_bind7 map (name, ty1, ty2, ty3, ty4) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty1, "width"); (ty2, "height"); (ty3, "xOffset"); (ty4, "yOffset")];
      locals = []; fbody = [] } map
    in List.fold_left add_bind7 func_map6 [
                            ("add_canvas", Num, Num, Num, Num);]
  in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  (*let function_decls = List.fold_left add_func functions*)
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
             NumLit l   -> (Num, SNumLit l)
           | ArrayLit l ->

        let test = string_of_int (List.length l) in
        (Array (Num, NumLit(test)), SArrayLiteral(List.map expr l, Array(Num, IntLiteral(List.length l))))

      | IntLiteral l -> (Num, SIntLiteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StrLit l   -> (String, SStrLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Access (s1, s2) -> (type_of_identifier s2, SAccess(s1,s2))
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Construct(cn, args) as construct ->
          (* Add class name to symbol table *)
          let add_class map cd =
            let dup_err = "duplicate class " ^ cd.cname
            and make_err er = raise (Failure er)
            and n = cd.cname (* Name of the class *)
            in match cd with (* No duplicate classes or redefinitions of built-ins *)
               | _ when StringMap.mem n map -> make_err dup_err
               | _ ->  StringMap.add n cd map
          in

          (* Collect all class names into one symbol table *)
          (* TODO: Fix the unused class_decls here *)
          let class_decls = List.fold_left add_class StringMap.empty classes
          in

          let find_class s =
            try StringMap.find s class_decls
            with Not_found -> raise (Failure ("unrecognized class " ^ s))
          in

          let _class = find_class cn in (* Ensure class cn is defined *)
          (*_class.ctformals *)
          let ct = List.nth _class.cdconst 0 in
          let formals = ct.ctformals in
          let param_length = List.length formals in
          let empty = StringMap.empty in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr construct ))
          else let check_const_call (formal_typ,_) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ formal_typ ^ " in " ^ string_of_expr e
            in (check_assign formal_typ et err, e')
          in
          let args' = List.map2 check_const_call formals args
          in (Object({className = cn; instanceVars = [] ;}) , SConstruct(cn, args'))

      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
        (*    Neg when t = Num || t = Float -> t *)
            Neg when t = Num -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Crementop(e, op) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            PreInc  when t = Num -> Num
          | PostInc when t = Num -> Num
          | PreDec  when t = Num -> Num
          | PostDec when t = Num -> Num
          | _ -> raise (Failure ("illegal increment/decrement operator " ^
                                   string_of_op op ^ string_of_typ t ^
                                   " in " ^ string_of_expr ex))
          in (ty, SCrementop((t, e'), op))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Num   -> Num
         (* | Add | Sub | Mult | Div when same && t1 = Float -> Float *)
          | Equal | Neq            when same               -> Bool
        (*  | Less | Leq | Greater | Geq
                     when same && (t1 = Num || t1 = Float) -> Bool *)
          |  Less | Leq | Greater | Geq
                       when same && (t1 = Num) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sfbody = match check_stmt (Block func.fbody) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in

  (**** Check Classes ****)

  (* Collect class declarations for built-in classes: no bodies *)

  let built_in_class_decls =
      let add_bind map (name, _, _) = StringMap.add name {
          cname = name;
          cdvars = [];
          cdconst = []; cdfuncs = [] } map
      in let func_map = List.fold_left add_bind StringMap.empty [
                                       ("point", Num, Num);
                                       ("ass_circle", Pt, Num);]

      in
      let add_bind2 map (name) = StringMap.add name {
          cname = name;
          cdvars = [];
          cdconst = []; cdfuncs = [] } map
      in List.fold_left add_bind2 func_map []
  in

  (* Add class name to symbol table *)
  let add_class map cd =
    let built_in_err = "class " ^ cd.cname ^ " may not be defined"
    and dup_err = "duplicate class " ^ cd.cname
    and make_err er = raise (Failure er)
    and n = cd.cname (* Name of the class *)
    in match cd with (* No duplicate classes or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_class_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n cd map
  in

  (* Collect all class names into one symbol table *)
  (* TODO: Fix the unused class_decls here *)
  let class_decls = List.fold_left add_class built_in_class_decls classes
  in

  (* Return a class from our symbol table *)
  (* let find_class s =
    try StringMap.find s class_decls
    with Not_found -> raise (Failure ("unrecognized class " ^ s))
  in *)

 (* let _ = find_class "main" in (* Ensure "main" is defined *) *)

  let check_class _class =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "cdvar" _class.cdvars;


    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let class_symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ _class.cdvars )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s class_symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        NumLit l   -> (Num, SNumLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StrLit l   -> (String, SStrLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
        (*    Neg when t = Num || t = Float -> t *)
            Neg when t = Num -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Num   -> Num
         (* | Add | Sub | Mult | Div when same && t1 = Float -> Float *)
          | Equal | Neq            when same               -> Bool
        (*  | Less | Leq | Greater | Geq
                     when same && (t1 = Num || t1 = Float) -> Bool *)
          |  Less | Leq | Greater | Geq
                       when same && (t1 = Num) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (**** Check Constructors ****)

    (* Collect function declarations for built-in functions: no bodies *)

    let built_in_const_decls =
      let add_bind map (name, _) = StringMap.add name {
        ctformals = [];
        ctlocals = []; ctbody = [] } map
      in List.fold_left add_bind StringMap.empty []
    in

    (* Add function name to symbol table *)
    let add_const map ctd =
      let built_in_err = "constructor may not be defined"
      and dup_err = "duplicate constructor"
      and make_err er = raise (Failure er)
      and n = ""
      in match ctd with (* No duplicate functions or redefinitions of built-ins *)
           _ when StringMap.mem n built_in_const_decls -> make_err built_in_err
         | _ when StringMap.mem n map -> make_err dup_err
         | _ ->  StringMap.add n ctd map
    in

    (* Collect all function names into one symbol table *)
    let constructor_decls = List.fold_left add_const built_in_const_decls _class.cdconst
    (*let function_decls = List.fold_left add_func functions*)
    in

    (* Return a function from our symbol table *)
    (* let find_const s =
      try StringMap.find s constructor_decls
      with Not_found -> raise (Failure ("unrecognized constructor " ^ s))
    in *)

    let check_constructor const =
      (* Make sure no formals or locals are void or duplicates *)
      check_binds "ctformal" const.ctformals;
      check_binds "ctlocal" const.ctlocals;

      (* Raise an exception if the given rvalue type cannot be assigned to
         the given lvalue type *)
      let check_assign lvaluet rvaluet err =
         if lvaluet = rvaluet then lvaluet else raise (Failure err)
      in

      (* Build local symbol table of variables for this function *)
      let local_symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                      StringMap.empty (globals @ const.ctformals @ const.ctlocals )
      in

      (* Return a variable from our local symbol table *)
      let type_of_identifier s =
        try StringMap.find s (StringMap.merge (fun k xo yo -> match xo,yo with
        | Some x, Some y -> Some (x)
        | None, yo -> yo
        | xo, None -> xo
        ) class_symbols local_symbols)
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
      in

      (* Return a semantically-checked expression, i.e., with a type *)
      let rec expr = function
              | ArrayLit l -> (Array (Num, IntLiteral(List.length l)), SArrayLiteral(List.map expr l, Array(Num, IntLiteral(List.length l))))
        | IntLiteral l -> (Num, SIntLiteral l)
        | NumLit l   -> (Num, SNumLit l)
        | BoolLit l  -> (Bool, SBoolLit l)
        | StrLit l   -> (String, SStrLit l)
        | Noexpr     -> (Void, SNoexpr)
        | Id s       -> (type_of_identifier s, SId s)
        | Access (s1, s2) -> (type_of_identifier s2, SAccess(s1, s2))
        | Assign(var, e) as ex ->
            let lt = type_of_identifier var
            and (rt, e') = expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
              string_of_typ rt ^ " in " ^ string_of_expr ex
            in (check_assign lt rt err, SAssign(var, (rt, e')))
        | Unop(op, e) as ex ->
            let (t, e') = expr e in
            let ty = match op with
          (*    Neg when t = Num || t = Float -> t *)
              Neg when t = Num -> t
            | Not when t = Bool -> Bool
            | _ -> raise (Failure ("illegal unary operator " ^
                                   string_of_uop op ^ string_of_typ t ^
                                   " in " ^ string_of_expr ex))
            in (ty, SUnop(op, (t, e')))
        | Crementop(e, op) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            PreInc  when t = Num -> Num
          | PostInc when t = Num -> Num
          | PreDec  when t = Num -> Num
          | PostDec when t = Num -> Num
          | _ -> raise (Failure ("illegal increment/decrement operator " ^
                                   string_of_op op ^ string_of_typ t ^
                                   " in " ^ string_of_expr ex))
          in (ty, SCrementop((t, e'), op))
        | Binop(e1, op, e2) as e ->
            let (t1, e1') = expr e1
            and (t2, e2') = expr e2 in
            (* All binary operators require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
              Add | Sub | Mult | Div when same && t1 = Num   -> Num
           (* | Add | Sub | Mult | Div when same && t1 = Float -> Float *)
            | Equal | Neq            when same               -> Bool
          (*  | Less | Leq | Greater | Geq
                       when same && (t1 = Num || t1 = Float) -> Bool *)
            |  Less | Leq | Greater | Geq
                         when same && (t1 = Num) -> Bool
            | And | Or when same && t1 = Bool -> Bool
            | _ -> raise (
            Failure ("illegal binary operator " ^
                         string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                         string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
      in

      let check_bool_expr e =
        let (t', e') = expr e
        and err = "expected Boolean expression in " ^ string_of_expr e
        in if t' != Bool then raise (Failure err) else (t', e')
      in

      (* Return a semantically-checked statement i.e. containing sexprs *)
      let rec check_stmt = function
          Expr e -> SExpr (expr e)
        | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
        | For(e1, e2, e3, st) ->
        SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
        | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)

          (* A block is correct if each statement is correct and nothing
             follows any Return statement.  Nested blocks are flattened. *)
        | Block sl ->
            let rec check_stmt_list = function
                [Return _ as s] -> [check_stmt s]
              | Return _ :: _   -> raise (Failure "nothing may follow a return")
              | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
              | s :: ss         -> check_stmt s :: check_stmt_list ss
              | []              -> []
            in SBlock(check_stmt_list sl)
        | _ -> raise (Failure ("Not semantically valid. You might've returned in a constructor."))

      in (* body of check_function *)
      {
        sctformals = const.ctformals;
        sctlocals  = const.ctlocals;
        sctbody = match check_stmt (Block const.ctbody) with
      SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
      }
    in (globals, List.map check_constructor _class.cdconst);

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)


	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_class *)

    {
      scname = _class.cname;
      scdvars = _class.cdvars;
      scdconst = List.map check_constructor _class.cdconst;
      scdfuncs = List.map check_function _class.cdfuncs
    }
  in (globals, List.map check_function functions, List.map check_class classes)
