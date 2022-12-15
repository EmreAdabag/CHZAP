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
  and i8_t       = L.i8_type     context
  and f64_t      = L.double_type context (* prob with floats *)
  and void_t     = L.void_type   context
  and char_t     = L.i8_type     context
  and i1_t       = L.i1_type     context
  in

  (* TODO: add more types *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

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

  (* Return the address of a variable from symbol table *)
  let addr_of_identifier (s : string) globalvars localvars : L.llvalue = 
    if Hashtbl.mem localvars s then Hashtbl.find localvars s 
    else Hashtbl.find globalvars s
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
  
  (* TODO: fix *)
  let main = L.define_function "main" (L.function_type i32_t (Array.of_list [])) the_module in

  (* Construct code for an expression; return its value *)
  let rec build_expr (globalvars : tbl_typ) (localvars : tbl_typ) builder ((_, e) : sexpr) = match e with
    | SIntLit(i)          -> L.const_int i32_t i
    | SBoolLit(b)         -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit(c)         -> L.const_int char_t (Char.code c)
    | SFloatLit(f)        -> L.const_float f64_t f
    | SArrayLit(l)        -> raise (Failure ("Arr not implemented"))
    | SId(s)       -> L.build_load (addr_of_identifier s globalvars localvars) s builder
      (* let var = addr_of_identifier s globalvars localvars in
      ignore(print_endline (L.string_of_llvalue var));
      (try L.build_load var s builder with e -> ignore(print_endline (L.string_of_llvalue var)); var) *)
    | SBinop(e1, op, e2) ->
      let e1' = build_expr globalvars localvars builder e1
      and e2' = build_expr globalvars localvars builder e2 in
      if (fst e1 = A.Int) || (fst e2 = A.Int) then
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mul     -> L.build_mul
        | A.Exp     -> L.build_mul (*TODO: fix*)
        | A.Div     -> L.build_sdiv
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
      else 
          (match op with
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mul     -> L.build_fmul
        | A.Exp     -> L.build_fmul (*TODO: fix*)
        | A.Div     -> L.build_fdiv
        | A.Mod     -> L.build_frem
        | A.BWAnd   -> L.build_and (*TODO: fix*)
        | A.BWOr    -> L.build_or (*TODO: fix*)
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      ) e1' e2' "tmp" builder
    | SUnop(op, e) -> 
      let e' = build_expr globalvars localvars builder e in
      (match op with
        A.Not   -> L.build_not
      ) e' "tmp" builder
    | SAssign(var, e) -> 
      let e' = build_expr globalvars localvars builder e in
      let v = addr_of_identifier var globalvars localvars in
      ignore(L.build_store e' v builder); e'
    | SSubscription(_, _) -> raise (Failure ("TODO"))
    | SCall ("print", [e]) ->
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
      L.build_call printf_func [| int_format_str ; (build_expr globalvars localvars builder e) |]
        "printf" builder
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
      let create_var (A.Bind(t, n) : A.bind) p = 
        let local = L.build_alloca (ltype_of_typ t) n fbuilder in
        let _ = L.build_store p local fbuilder in
        
        Hashtbl.add locals n local
      in
      let _ = List.map2 create_var bl (Array.to_list (L.params the_function)) in
      (* build stmt *)
      let rbuilder = build_stmt globals locals fbuilder s in
      (* add return if not: void *)
      ignore(add_terminal rbuilder (L.build_ret (L.const_int void_t 0)));
      (* return the function *)
      the_function
  
  (* Build the code for the given statement; return the builder for
    the statement's successor (i.e., the next instruction will be built
    after the one generated by this call) *)
    and build_stmt (globalvars : tbl_typ) (localvars : tbl_typ) builder =
  (* TODO: fix *)
    let the_function = main in
    function
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

    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = build_expr globalvars localvars builder predicate in

      let then_bb = L.append_block context "then" the_function in
      ignore (build_stmt globalvars localvars (L.builder_at_end context then_bb) then_stmt);
      let else_bb = L.append_block context "else" the_function in
      ignore (build_stmt globalvars localvars (L.builder_at_end context else_bb) else_stmt);
      let end_bb = L.append_block context "if_end" the_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      add_terminal (L.builder_at_end context else_bb) build_br_end;
      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context end_bb

    | SFor (e1, e2, e3, body) -> 
      build_stmt globalvars localvars builder ( SBlock [e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )

    | SWhile (predicate, body) ->
      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in (* partial function *)
      ignore (build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr globalvars localvars while_builder predicate in
      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (build_stmt globalvars localvars (L.builder_at_end context body_bb) body) build_br_while;
      let end_bb = L.append_block context "while_end" the_function in
      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb
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
      let create_var (A.Bind(t, n) : A.bind) p = 
        (match t with
        | Ftyp(t, tl) ->
          (* get the function *)
          let ft = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl)) in
          let local_func = L.declare_function n ft the_module in
          Hashtbl.add locals n local_func
        | _ ->
          let local = L.build_alloca (ltype_of_typ t) n fbuilder in
          let _ = L.build_store p local fbuilder in
          Hashtbl.add locals n local)
      in
      let _ = List.map2 create_var bl (Array.to_list (L.params the_function)) in
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
  let builder = L.builder_at_end context (L.entry_block main) in
  let ret = build_stmt_list globals locals builder program in
  let _ = add_terminal ret (L.build_ret (L.const_int i32_t 0)) in

  (* return the module *)
  the_module
