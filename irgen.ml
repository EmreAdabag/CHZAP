(* IR generation: translate takes a semantically checked AST and
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


type tbl_typ = (string, L.llvalue) Hashtbl.t

(* translate : Sast.program -> Llvm.module *)
let translate (program : sstmt list) : Llvm.llmodule = 

  (* llvm envs *)
  let context    = L.global_context () in
  let the_module = L.create_module context "CHZAP" in

  (* llvm types *)
  let i32_t      = L.i32_type    context
  (* and i8_t       = L.i8_type     context *)
  and f64_t      = L.double_type context (* prob with floats *)
  and void_t     = L.void_type   context
  and char_t     = L.i8_type     context
  and i1_t       = L.i1_type     context
  in

  (* Return the address of a variable from symbol table *)
  let addr_of_identifier (s : string) (globalvars : tbl_typ) (localvars : tbl_typ) : L.llvalue = 
    if Hashtbl.mem localvars s then Hashtbl.find localvars s 
    else Hashtbl.find globalvars s
  in

  (* return llvm type for sast type *)
  let rec ltype_of_typ = function
    | A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f64_t
    | A.Void -> void_t 
    | A.Char -> char_t
    (* type checks are the job of semantics *)
    | A.Const(t) -> ltype_of_typ t
    | A.Arr(_) -> raise (Failure ("Arr not implemented"))
    | A.Ftyp(t, tl) -> L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl))
    | A.Dyn -> raise (Failure ("Dyn not implemented"))
  in

  (* Return a list of types from a list of bindings *)
  let rec types_of_binds (binds : A.bind list) : A.typ list =
    match binds with
    | [] -> []
    | A.Bind(x, _) :: t -> x :: types_of_binds t
  in

  let ftype_of_binds (Bind(rt, _) : A.bind) (bl : A.bind list) =
    L.function_type (ltype_of_typ rt) (Array.of_list (List.map ltype_of_typ (types_of_binds bl)))
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder) in

  (* Construct code for an expression; return its value *)
  let rec build_expr (globalvars : tbl_typ) (localvars : tbl_typ) builder ((_, e) : sexpr) = match e with
    | SIntLit(i)          -> L.const_int i32_t i
    | SBoolLit(b)         -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit(c)         -> L.const_int char_t (Char.code c)
    | SFloatLit(f)        -> L.const_float f64_t f
    | SArrayLit(l)        -> raise (Failure ("Arr not implemented"))
    | SId(s) -> 
      let var = addr_of_identifier s globalvars localvars in
      ignore(print_endline (L.string_of_llvalue var));
      (try L.build_load var s builder with e -> ignore(print_endline (L.string_of_llvalue var)); var)
    | SBinop(e1, op, e2) ->
      let e1' = build_expr globalvars localvars builder e1
      and e2' = build_expr globalvars localvars builder e2 in
      (match op with
      | A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mul     -> L.build_mul
      | A.Exp     -> L.build_mul (*TODO: fix*)
      | A.Div     -> L.build_sdiv (*TODO: type*)
      | A.Mod     -> L.build_srem
      | A.BWAnd   -> L.build_and (*TODO: fix*)
      | A.BWOr    -> L.build_or (*TODO: fix*)
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Eq      -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
    | SUnop(op, e) -> raise (Failure ("Unop not implemented"))
    | SAssign(var, e) -> 
      let e' = build_expr globalvars localvars builder e in
      let v = addr_of_identifier var globalvars localvars in
      (* handle first order functions *)
      ignore(L.build_store e' v builder); e'
    | SSubscription(_, _) -> raise (Failure ("Subscription not implemented"))
    (* | SCall ("print", [e]) ->
      L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
        "printf" builder *)
    | SCall(f, args) -> 
      let the_function = addr_of_identifier f globalvars localvars in
      let llargs = List.rev (List.map (build_expr globalvars localvars builder) (List.rev args)) in
      let result = f ^ "_result" in
      L.build_call the_function (Array.of_list llargs) result builder
    | SNoexpr -> L.const_int void_t 0
    | SAfunc(rt, bl, s) -> 
      (* define the function, give it a default name *)
      let fname = "_anonymou" in
      let the_function = L.define_function fname (ftype_of_binds (Bind(rt, fname)) bl) the_module in
      (* we don't store anonymous functions to symbol tables upon definition *)
      (* get the builder *)
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      (* create a new "global" scope with variables outside of current scope *)
      let globals : tbl_typ = Hashtbl.copy globalvars in
      let _ = Hashtbl.iter (fun k v -> Hashtbl.add globals k v) localvars in
      (* create a "local" scope with all formals *)
      let locals : tbl_typ = Hashtbl.create 1000 in
      let create_var (A.Bind(t, n) : A.bind) = 
        let local = L.build_alloca (ltype_of_typ t) n fbuilder in
        Hashtbl.add locals n local
      in
      let _ = List.map create_var bl in
      (* build stmt *)
      let rbuilder = build_stmt globals locals fbuilder s in
      (* add return if not: void *)
      ignore(add_terminal rbuilder (L.build_ret (L.const_int void_t 0)));
      (* return the function *)
      the_function

  (* Build the code for the given statement; return the builder for
    the statement's successor (i.e., the next instruction will be built
    after the one generated by this call) *)
  and build_stmt (globalvars : tbl_typ) (localvars : tbl_typ) builder = function
    (* allocate var and add address to locals *)
    | SBstmt(Bind(t, n)) -> 
      (* handle function type binding *)
      (match t with
      (* is a function *)
      | A.Ftyp(t, tl) -> 
        let ft = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl)) in
        let the_function = L.define_function n ft the_module in
        let _ = Hashtbl.add localvars n the_function in
        builder
      (* is a value *)
      | _ ->
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore(Hashtbl.add localvars n local); builder)
    | SBAstmt(b, e) -> 
      (* bind and assign *)
      let b = build_stmt globalvars localvars builder (SBstmt(b)) in
      ignore(build_expr globalvars localvars b (A.Void, e)); builder
    | SBlock(sl) -> build_stmt_list globalvars localvars builder sl
    | SExpr(e) -> ignore(build_expr globalvars localvars builder e); builder
    | SIf (predicate, then_stmt, else_stmt) -> raise (Failure ("If not implemented"))
      (* let bool_val = build_expr builder predicate in
      let then_bb = L.append_block context "then" the_function in
      ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
      let else_bb = L.append_block context "else" the_function in
      ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);
      let end_bb = L.append_block context "if_end" the_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      add_terminal (L.builder_at_end context else_bb) build_br_end;
      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context end_bb *)
    | SFor (e1, e2, e3, body) -> raise (Failure ("For not implemented"))(*build_stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )*)
    | SWhile (predicate, body) -> raise (Failure ("While not implemented"))
      (* let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in (* partial function *)
      ignore (build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr while_builder predicate in
      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;
      let end_bb = L.append_block context "while_end" the_function in
      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb *)
    | SContinue -> raise (Failure ("CodegenError: Continue has not been implemented for codegen"))
    | SBreak -> raise (Failure ("CodegenError: Break has not been implemented for codegen"))
    | SReturn e -> ignore(L.build_ret (build_expr globalvars localvars builder e) builder); builder
    | SFunc(b, bl, s) -> 
      (* define the function *)
      let A.Bind(rt, fname) = b in
      let the_function = L.define_function fname (ftype_of_binds b bl) the_module in
      (* store the function in global symbol table *)
      let _ = Hashtbl.add globalvars fname the_function in
      (* get the builder *)
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      (* create a new "global" scope with variables outside of current scope *)
      let globals : tbl_typ = Hashtbl.copy globalvars in
      let _ = Hashtbl.iter (fun k v -> Hashtbl.add globals k v) localvars in
      (* create a "local" scope with all formals *)
      let locals : tbl_typ = Hashtbl.create 1000 in
      let create_var (A.Bind(t, n) : A.bind) = 
        let local = L.build_alloca (ltype_of_typ t) n fbuilder in
        Hashtbl.add locals n local
      in
      let _ = List.map create_var bl in
      (* build stmt *)
      let rbuilder = build_stmt globals locals fbuilder s in
      (* add return if not: void *)
      ignore(add_terminal rbuilder (L.build_ret (L.const_int void_t 0)));
      (* return the original builder *)
      builder

  and build_stmt_list (globalvars : tbl_typ) (localvars : tbl_typ) builder = function
  | [] -> builder
  | s :: sl -> 
    let b = build_stmt globalvars localvars builder s in
    build_stmt_list globalvars localvars b sl
  in

  (* symbol tables *)
  let (globals : tbl_typ) = Hashtbl.create 1000 in
  let (locals : tbl_typ) = Hashtbl.create 1000 in

  (* main block *)
  let main = L.define_function "main" (L.function_type i32_t (Array.of_list [])) the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let ret = build_stmt_list globals locals builder program in
  ignore(add_terminal ret (L.build_ret (L.const_int i32_t 0)));

  (* return the module *)
  the_module

(*
module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and f64_t  = L.double_type context (* prob with floats *)
  and void_t = L.void_type context
  and char_t     = L.i8_type     context
  and i1_t       = L.i1_type     context in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f64_t
    | A.Void -> void_t 
    | A.Char -> char_t
    | A.Arr(_) -> void_t (*TODO*) 
    | A.Const(_) -> void_t (*TODO*)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

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
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mul   -> L.build_mul
         | A.Exp      -> L.build_mul (*TODO: fix*)
         | A.Div    -> L.build_sdiv (*TODO: type*)
         | A.Mod    -> L.build_srem
         | A.BWAnd -> L.build_and (*TODO: fix*)
         | A.BWOr -> L.build_or (*TODO: fix*)
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Eq   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq     -> L.build_icmp L.Icmp.Sge

        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      | SNoexpr -> raise (Failure ("TODO"))
      | SCharLit l -> raise (Failure ("TODO"))
      | SFloatLit l -> raise (Failure ("TODO"))
      | SArrLit l -> raise (Failure ("TODO"))
      | SUnop(_, _) -> raise (Failure ("TODO"))
      | SSubsription(_, _) -> raise (Failure ("TODO"))
      
      
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
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      | SFor (e1, e2, e3, body) -> build_stmt builder
	      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      | SContinue -> raise (Failure ("CodegenError: Continue has not been implemented for codegen"))
      | SBreak -> raise (Failure ("CodegenError: Break has not been implemented for codegen"))
    in

    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
*)