(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions', classes) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Bugsy" in
  
  let convert_int = function
          i -> 4
  in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and ye_t       = L.float_type  context
  and string_t   = L.pointer_type (L.i8_type context) (*new string type *)
  and array_t    = L.array_type
  and void_t     = L.void_type   context
  and obj_t      = L.named_struct_type context (*uninitialized struct *)
  and struct_t arr = L.struct_type arr
  (*and obj_ptr_t = L.pointer_type styp 
  and obj_ref_t = L.named_struct_type context name 
  in 
  L.struct_set_body obj_ref_t [|i32_t; obj_ptr_t|] false;
*)
  in

  (* Return the LLVM type for a Bugsy type *)
  let rec ltype_of_prim_typ = function
      A.Num   -> float_t
    | A.Bool  -> i1_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.Int -> i32_t
    | A.Array(typ, size) -> (match typ with
          A.Num -> array_t float_t (convert_int size))
    | A.Object(classTyp) -> obj_t classTyp.A.className
  in 

  (*go through all functions, find main, and change main to return int *)
  let functions = List.map (fun x -> if x.sfname = "main" then ((x.styp <- A.Int); x) else x) functions'
  in

(*** BEGIN CLASS TABLE ***)
  (* CLASS LOOKUP TABLE *)
  (* maps class name to (classtype ptr, instance var) tuple *)
  let class_table = 
    let class_decl m cdecl = 
      let name = cdecl.scname
      and instance_var_types = Array.of_list 
        (List.map (fun (t,_) -> ltype_of_prim_typ t) (cdecl.scdvars)) in 
      let class_type = L.struct_type context instance_var_types in
      StringMap.add name (class_type, cdecl.scdvars) m 
    in 
    List.fold_left class_decl StringMap.empty classes
  in 

  (* CLASS LOOKUP FUNC *)
  let find_class n = try StringMap.find n class_table
    with Not_found -> raise (Failure ("Error: class " ^ n ^ " not defined"))
  in
(*** END CLASS TABLE ***)

  let rec ltype_of_typ = function
    A.Object({className; instanceVars} as ct) -> (find_class ct.A.className)
    | t -> ltype_of_prim_typ t
  in

  (* Create a map of global variables after creating each *)

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          A.Num -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in


(***********************************************************
 *              BEGIN BUILTIN DEFINITIONS                  *
 **********************************************************)

  let printf_t : L.lltype =
      L.var_arg_function_type float_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in
  let demo_t : L.lltype =
      L.function_type float_t [||] in
  let demo_func : L.llvalue =
      L.declare_function "demo" demo_t the_module in

  let add_point_xy_t : L.lltype =
      L.function_type float_t [| float_t; float_t; |] in
  let add_point_xy_func : L.llvalue =
      L.declare_function "add_point_xy" add_point_xy_t the_module in

  let circle_t : L.lltype =
      L.function_type float_t [| float_t; float_t; float_t |] in
  let circle_func : L.llvalue =
      L.declare_function "add_circle" circle_t the_module in

  let square_t : L.lltype =
      L.function_type float_t [| float_t; float_t; float_t; L.pointer_type i8_t; L.pointer_type i8_t |] in
  let square_func : L.llvalue =
      L.declare_function "add_square" square_t the_module in

  let canvas_t : L.lltype =
      L.function_type float_t [| float_t; float_t; float_t; float_t |] in
  let canvas_func : L.llvalue =
      L.declare_function "add_canvas" canvas_t the_module in

(**********************************************************)
(*          END BUILTIN FUNCTION DECLARATIONS             *)
(**********************************************************)

(**********************************************************)
(*              END CONSTRUCTOR DECLARATIONS              *)
(**********************************************************)
(***********************************************************
 *                  BEGIN METHOD LIFTING                   *
 *                                                         *
 *  We need to massage the class functions and do method   *
 *  lifting to bring them into global scope, then we can   *
 *  use the same method for initializing their bodies      *
 *  this means taking fname and prepending the class name  *
 *  and adding the struct type to the formal params        *
 **********************************************************)
  let class_functions = 
    let method_lifter cdecl = 
      {
        typ = cdecl.scdfunc.typ;
        fname = cdecl.scname ^ "_" ^ cdecl.scdfunc.sfname;
        formals = cdecl.scdfunc.sformals;
        (*TODO: Add self to the formals*)
        locals = cdecl.scdfunc.slocals;
        body = cdecl.scdfunc.sfbody;
      } 
    in List.map method_lifter classes 
  in 

(**********************************************************)
(*                   LIFT CONSTRUCTORS                    *)
(**********************************************************)

  let constructors = 
    let constructor_lifter cdecl =
      (* get the constructor from the class *) 
      let ct = List.nth 0 cdecl.scdconst in

      (* get the constructor's name *) 
      let cname = cdecl.scname in
      let name = cname ^ "_constructor"
      
      (* get types of formals from the constructor *)
      and formal_types =
    Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) cdecl.sctformals)
      in
      (* turn the constructor into an fdecl *)
      {
        (* build the struct ptr we need to return as the return type *)
        typ = L.pointer_type (L.struct_type context formal_types) 
        fname = name; 
        formals = ct.sctformals;
        locals = ct.sctlocals;
        body = ct.sctbody;
      }
    in L.map constructor_lifter classes
  in (* return the list of constructors *)


(**********************************************************)
(*                  END METHOD LIFTING                    *)
(**********************************************************)

(**********************************************************)
(*               BEGIN FUNCTION DECLARATIONS              *)
(**********************************************************)

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in 
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      (*if function is equal to main, rather than adding ftype, add int type *)

      let ret_int = L.function_type (ltype_of_typ A.Int) formal_types in

   (*   if name = "main" then let ret_type = int in else let ret_type = ftype in *)
      let ret_type = if name = "main" then ret_int else ftype in
      StringMap.add name (L.define_function name ret_type  the_module, fdecl) m 
    in
    List.fold_left function_decl StringMap.empty (
      List.append (
        (List.append functions class_functions) constructors)
    ) 
  in



  (* FUNC LOOKUP *) 
  let find_func s =
          (*let test_1 = StringMap.update "main" (L.define_function "main" i32_t the_module, fdecl) in *)

          try begin StringMap.find s function_decls; end

    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)




  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    (*add string formatting here too stuff like %s *)
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

       (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SStrLit s -> L.build_global_stringptr s "str" builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SNumLit nl -> L.const_float_of_string float_t nl
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SArrayLiteral (l, t) -> L.const_array (ltype_of_typ t) (Array.of_list (List.map (expr builder) l))
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'

      (*a is name of class, e is expression list   *)                    
      | SConstruct(cn , args) -> List.iter (fun e -> let test =  expr builder e in L.dump_value(test)) args; let (ctdef, ctdecl) = StringMap.find cn const_decls in 
      
      let llargs = List.rev (List.map (expr builder) (List.rev args)) in 
      L.build_call ctdef (Array.of_list llargs) "tester" builder;
      
      raise (Failure ("f"))

      | SClassCall(c, f, el) -> raise (Failure ("SClassCall not ready yet"))
      | SAccess(c, v) -> raise (Failure ("SAccess not ready yet"))
      | SCrementop(e, op) ->
          let e' = expr builder e in
          let one = expr builder (A.Num, (Sast.SNumLit "1.0")) in
          let eplus = L.build_fadd e' one "tmp" builder
          and eminus = L.build_fsub e' one "tmp" builder
          and s = (match snd e with
            SId s -> s
            | _ -> raise (Failure ("assignment failed")))
          in
          (match op with
            A.PreInc  -> ignore(L.build_store eplus  (lookup s) builder); eplus
          | A.PostInc -> ignore(L.build_store eplus  (lookup s) builder); e'
          | A.PreDec  -> ignore(L.build_store eminus (lookup s) builder); eminus
          | A.PostDec -> ignore(L.build_store eminus (lookup s) builder); e'
          )

      | SBinop ((A.Num,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
      A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg when t = A.Num -> L.build_fneg
	  | A.Neg                -> L.build_neg
    | A.Not                -> L.build_not) e' "tmp" builder
    | SCall ("printb", [e]) ->
      L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
    | SCall ("print", [e]) ->
	    L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
    | SCall ("demo", []) ->
  	  L.build_call demo_func [||] "demo" builder
    (* | SCall ("printbig", [e]) ->
	    L.build_call printbig_func [| (expr builder e) |] "printbig" builder *)
    | SCall ("printf", [e]) ->
	    L.build_call printf_func [| string_format_str ; (expr builder e) |]
	    "printf" builder
    | SCall ("add_point_xy", [e1; e2]) ->
      L.build_call add_point_xy_func [| (expr builder e1); (expr builder e2); |]
      "add_point_xy" builder
    | SCall ("add_circle", [e1; e2; e3]) ->
      L.build_call circle_func [| (expr builder e1); (expr builder e2); (expr builder e3);|]
      "add_circle" builder
    | SCall ("add_square", [e1; e2; e3]) ->
      L.build_call square_func [| (expr builder e1); (expr builder e2); (expr builder e3);|]
      "add_square" builder
    | SCall ("add_canvas", [e1; e2; e3; e4]) ->
      L.build_call canvas_func [| (expr builder e1); (expr builder e2); (expr builder e3); (expr builder e4);|]
      "add_canvas" builder
    | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                             |
                              A.Int -> L.build_ret (L.const_null i32_t) builder
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sfbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Num -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
(**********************************************************)
(*                END FUNCTION DECLARATIONS               *)
(**********************************************************)

(**********************************************************
 * We need to make sure the class functions 
 * exist, then the constructor bodies,
 * then the outermost global functions 
 *********************************************************)

  List.iter build_function_body class_functions;
  List.iter build_function_body classes;
  List.iter build_function_body functions;
  the_module
