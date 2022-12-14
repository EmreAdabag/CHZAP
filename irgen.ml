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


type sbl_tbl = (string, L.llvalue) Hashtbl.t
type mem_rec = {n_ref: int; size: int}
type mem_tbl = (L.llvalue, mem_rec) Hashtbl.t

(* translate : Sast.program -> Llvm.module *)
let translate (program : sstmt list) : Llvm.llmodule = 

  (* llvm envs *)
  let context    = L.global_context () in
  let the_module = L.create_module context "CHZAP" in

  (* llvm types *)
  let i32_t      = L.i32_type    context
  and f64_t      = L.double_type context (* prob with floats *)
  and void_t     = L.void_type   context
  and char_t     = L.i8_type     context
  and char_pt    = L.pointer_type (L.i8_type context)
  and i1_t       = L.i1_type     context
  in

  (* return llvm type for sast type *)
  let rec ltype_of_typ = function
    | A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f64_t
    | A.Void -> void_t 
    | A.Char -> char_t
    | A.String -> char_pt
    (* type checks are the job of semantics *)
    | A.Const(t) -> ltype_of_typ t
    | A.Arr(t, i) -> L.array_type (ltype_of_typ t) i
    | A.Darr(t) -> L.pointer_type (ltype_of_typ t)
    | A.Ftyp(t, tl) -> 
      let ft = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl)) in
      L.pointer_type ft
  in

  let remove_const (t : A.typ) : A.typ = match t with
    | A.Const(t') -> t'
    | _ -> t
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

  (* return a llvm function type from a list of bindings *)
  let ftype_of_binds (Bind(rt, _) : A.bind) (bl : A.bind list) =
    L.function_type (ltype_of_typ rt) (Array.of_list (List.map ltype_of_typ (types_of_binds bl)))
  in

  (* add return stmt if not *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder) 
  in

  (* printf *)
  let printf_t = L.var_arg_function_type i32_t [| char_pt |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* let print_i_t = L.var_arg_function_type i32_t [| i32_t |] in
  let print_i_func = L.declare_function "print_i" print_i_t the_module in *)

  (* let print_f_t = L.var_arg_function_type i32_t [| f64_t |] in
  let print_f_func = L.declare_function "print_f" print_f_t the_module in

  let print_b_t = L.var_arg_function_type i32_t [| char_t |] in
  let print_b_func = L.declare_function "print_b" print_b_t the_module in *)

  (* exit *)
  let exit_t = L.function_type (i32_t) [| i32_t |] in
  let exit_func = L.declare_function "exit" exit_t the_module in

  let bwAnd_t = L.function_type i32_t [| i32_t ; i32_t |] in
  let bwAnd_func = L.declare_function "bitwiseAnd" bwAnd_t the_module in

  let bwOr_t = L.function_type i32_t [| i32_t ; i32_t |] in
  let bwOr_func = L.declare_function "bitwiseOr" bwOr_t the_module in

  (* memory table: addr -> {'nref', 'size'} *)
  (* let (memory : mem_tbl) = Hashtbl.create 1000 in *)

  (* build print *)
  let rec build_print fmt sexpr globals locals builder = 
    let fmtp = L.build_global_stringptr fmt "fmt" builder in
    L.build_call printf_func [| fmtp ; build_expr globals locals builder sexpr |] "bark" builder

  (* build function arguments *)
  and build_arg globals locals builder = function
    | A.Ftyp(_, _), SId(fname) -> 
      let faddr = addr_of_identifier fname globals locals in
      L.build_load faddr (fname ^ "_ptr") builder
    | sx -> build_expr globals locals builder sx

  (* Construct code for an expression; return its value *)
  and build_expr (globalvars : sbl_tbl) (localvars : sbl_tbl) builder ((_, e) : sexpr) = match e with
    | SIntLit(i)          -> L.const_int i32_t i
    | SBoolLit(b)         -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit(c)         -> L.const_int char_t (Char.code c)
    | SStringLit(s)       -> 
      (* let clst = List.map (fun c -> L.const_int char_t (Char.code c)) (List.of_seq (String.to_seq s)) in
      let cstr = L.const_array char_t (Array.of_list clst) in
      let addr = L.build_alloca char_pt "_const_str" builder in
      ignore(L.build_store cstr addr builder);
      addr *)
      L.build_global_stringptr s "_string" builder
    | SFloatLit(f)        -> L.const_float f64_t f
    | SArrayLit (sexprs) -> 
      let size = List.length sexprs in
      let ltype_of_arr = ltype_of_typ (fst(List.hd sexprs)) in
      let all_emements = List.map (fun x -> build_expr globalvars localvars builder  x) sexprs in
      let this_array = L.build_alloca (L.array_type ltype_of_arr size) "tmp" builder in
      let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
      let index_list = range 0 size in
      List.iter (fun x ->
        let where = 
          L.build_in_bounds_gep this_array [| L.const_int i32_t 0; L.const_int i32_t x |] "tmp" builder 
        in
        let what = List.nth all_emements x in
        ignore (L.build_store what where builder)
      ) index_list; L.build_load this_array "tmp" builder

    | SId(s)       -> L.build_load (addr_of_identifier s globalvars localvars) s builder
      (* let var = addr_of_identifier s globalvars localvars in
      ignore(print_endline (L.string_of_llvalue var));
      (try L.build_load var s builder with e -> ignore(print_endline (L.string_of_llvalue var)); var) *)
    | SBinop(e1, op, e2) ->
      let e1' = build_expr globalvars localvars builder e1
      and e2' = build_expr globalvars localvars builder e2 in
      (match op with
        A.BWAnd -> 
          L.build_call bwAnd_func [|e1' ; e2'|] "bitwiseAnd" builder
        | A.BWOr    -> 
          L.build_call bwOr_func [|e1' ; e2'|] "bitwiseOr" builder
        | _ -> (
        if (fst e1 = A.Int) && (fst e2 = A.Int) then
          (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mul     -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure("impossible"))
          ) e1' e2' "tmp" builder
        else 
            (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _ -> raise (Failure("impossible"))
        ) e1' e2' "tmp" builder
        ))

    | SUnop(op, e) -> 
      let e' = build_expr globalvars localvars builder e in
      (match op with
        A.Not   -> L.build_not
      ) e' "tmp" builder
    | SAssign(var, e) -> 
      (match e with
      | A.Darr(_), _ -> raise (Failure ("TODO: assignment to dynamic array not implemented"))
      | _ ->
        let e' = build_expr globalvars localvars builder e in
        let v = addr_of_identifier var globalvars localvars in
        ignore(L.build_store e' v builder); e')
    | SSubscription(n, e) -> 
      let idx = build_expr globalvars localvars builder e in
      let addr = addr_of_identifier n globalvars localvars in
      let p = L.build_gep addr [| L.const_int i32_t 0; idx |] "tmp" builder in
      L.build_load p "elem" builder

    (* | SCall ("print", [(typ, _) as e]) ->
      let t = remove_const typ in
      ( match t with
      | Int -> L.build_call print_i_func [| build_expr globalvars localvars builder e |]
        "print_i" builder
      | Float -> L.build_call print_f_func [| build_expr globalvars localvars builder e |]
        "print_f" builder
      | Bool -> L.build_call print_b_func [| build_expr globalvars localvars builder e |]
        "print_b" builder
      | _ -> raise (Failure ("Print type " ^ Ast.string_of_typ typ ^ " not suppoted"))
      ) *)

    | SCall ("bark", args) ->
      (match args with
      | [] -> build_print "%c" (String, SCharLit('\n')) globalvars localvars builder
      | ((typ, _) as e) :: el -> 
        let t = remove_const typ in
        ignore(match t with
          | Int -> build_print "%d" e globalvars localvars builder
          | Float -> build_print "%f" e globalvars localvars builder
          | String -> build_print "%s" e globalvars localvars builder
          | _ -> raise (Failure ("Print type " ^ Ast.string_of_typ typ ^ "not suppoted")));
        ignore(build_print "%c" (String, SCharLit(' ')) globalvars localvars builder);
        build_expr globalvars localvars builder (A.Void, SCall("bark", el)))

    | SCall(f, args) -> 
      (* ignore(print_endline f); *)
      let addr = (addr_of_identifier f globalvars localvars) in
      (* ignore(print_endline (L.string_of_lltype (L.type_of addr))); *)
      let the_function = L.build_load addr (f ^ "_call") builder in
      (* ignore(print_endline (L.string_of_lltype (L.type_of the_function))); *)
      let llargs = List.rev (List.map (build_arg globalvars localvars builder) (List.rev args)) in
      (* ignore(List.map (fun x -> print_endline (L.string_of_llvalue x)) llargs); *)
      let result = f ^ "_result" in
      L.build_call the_function (Array.of_list llargs) result builder
    | SNoexpr -> L.const_int void_t 0
    | SAfunc(rt, bl, s) ->
      (* define the function, give it a default name *)
      let fname = "_anonymous" in
      let ft = ftype_of_binds (Bind(rt, fname)) bl in
      let the_function = L.define_function fname ft the_module in
      (* we don't store anonymous functions to symbol tables upon definition *)
      let fp = L.build_alloca (L.pointer_type ft) fname builder in
      ignore(L.build_store the_function fp builder);
      (* get the builder *)
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      (* create a new "global" scope with variables outside of current scope *)
      let globals : sbl_tbl = Hashtbl.copy globalvars in
      let _ = Hashtbl.iter (fun k v -> Hashtbl.add globals k v) localvars in
      (* create a "local" scope with all formals *)
      let locals : sbl_tbl = Hashtbl.create 1000 in
      let create_var (A.Bind(t, n) : A.bind) p = 
        let local = L.build_alloca (ltype_of_typ t) n fbuilder in
        let _ = L.build_store p local fbuilder in
        Hashtbl.add locals n local
      in
      let _ = List.map2 create_var bl (Array.to_list (L.params the_function)) in
      (* build stmt *)
      let rbuilder = build_stmt globals locals fbuilder the_function None s in
      (* add return if not: void *)
      let instr = 
        match rt with
        | A.Void -> L.build_ret_void
        | _ -> L.build_ret (L.const_int (ltype_of_typ rt) 0)
      in
      ignore(add_terminal rbuilder instr);
      (* return the function *)
      the_function
      (* fp *)
  
    (* Build the code for the given statement; return the builder for
      the statement's successor (i.e., the next instruction will be built
      after the one generated by this call) *)
    and build_stmt (globalvars : sbl_tbl) (localvars : sbl_tbl) builder the_function loop = function
    (* allocate var and add address to locals *)
    | SBstmt(Bind(t, n)) -> 
      (* handle function type binding *)
      let local = 
        (match t with
        (* is a function *)
        | A.Ftyp(t, tl) -> 
          let ft = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl)) in
          (* let the_function = L.define_function n ft the_module in *)
          L.build_alloca (L.pointer_type ft) n builder
        (* is a value *)
        | _ -> L.build_alloca (ltype_of_typ t) n builder)
      in
      ignore(Hashtbl.add localvars n local); 
      builder
    | SBAstmt(b, e) -> 
      (* bind and assign *)
      let b = build_stmt globalvars localvars builder the_function loop (SBstmt(b)) in
      ignore(build_expr globalvars localvars b (A.Void, e)); builder
    | SBlock(sl) -> build_stmt_list globalvars localvars builder the_function loop sl
    | SExpr(e) -> ignore(build_expr globalvars localvars builder e); builder

    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = build_expr globalvars localvars builder predicate in
      let end_bb = L.append_block context "end_if" the_function in
      let build_br_end = L.build_br end_bb in (* partial function *)

      let then_bb = L.append_block context "then" the_function in
      add_terminal (build_stmt globalvars localvars (L.builder_at_end context then_bb) the_function loop then_stmt) build_br_end;

      let else_bb = L.append_block context "else" the_function in
      add_terminal (build_stmt globalvars localvars (L.builder_at_end context else_bb) the_function loop else_stmt) build_br_end;
      
      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context end_bb

    | SFor (init, predicate, acc, body) -> 
      build_stmt globalvars localvars builder the_function loop ( 
        SBlock [
          init ; 
          SWhile (predicate, body, Some(SExpr acc)) 
        ])  

    | SWhile (predicate, body, accumulate) ->
      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in (* partial function *)
      ignore (build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr globalvars localvars while_builder predicate in
      let body_bb = L.append_block context "while_body" the_function in
      let end_bb = L.append_block context "while_end" the_function in

      (match accumulate with 
        Some(accumulate) -> 
          let acc_bb = L.append_block context "while_acc" the_function in
          let acc_builder = build_stmt globalvars localvars (L.builder_at_end context acc_bb) the_function (Some(acc_bb, end_bb)) accumulate in
          add_terminal (acc_builder) build_br_while; 
          let new_builder = build_stmt globalvars localvars (L.builder_at_end context body_bb) the_function (Some(acc_bb, end_bb)) body in
          add_terminal (new_builder) (L.build_br acc_bb); 
        | None ->
          let new_builder = build_stmt globalvars localvars (L.builder_at_end context body_bb) the_function (Some(while_bb, end_bb)) body in
          add_terminal (new_builder) build_br_while; 
         );
      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb
    | SContinue -> 
      (
        match loop with 
        | Some(while_bb, _) -> ignore(L.build_br while_bb builder); builder
        | None -> builder
      )
    | SBreak ->         
      (
          match loop with 
        | Some(_, end_bb) -> ignore(L.build_br end_bb builder); builder
        | None -> builder
      ) 

    | SAssert(e) -> 
      (* evaluate the expr *)
      let ret = build_expr globalvars localvars builder e in
      (* build assert struct *)
      let fail_bb = L.append_block context "assert_fail" the_function in
      let end_bb = L.append_block context "end_assert" the_function in
      let build_br_end = L.build_br end_bb in (* partial *)
      (* cond jmp *)
      ignore(L.build_cond_br ret end_bb fail_bb builder);
      (* if fail: exit with code 1 *)
      let fail_builder = L.builder_at_end context fail_bb in
      ignore(build_print "assertion failed: %d" e localvars globalvars fail_builder);
      ignore(L.build_call exit_func [| (L.const_int i32_t 1) |] "exit" fail_builder);
      ignore(add_terminal fail_builder build_br_end);
      (* if success *)
      L.builder_at_end context end_bb

    | SReturn e -> ignore(L.build_ret (build_expr globalvars localvars builder e) builder); builder
    | SFunc(b, bl, s) ->
       (* define the function *)
      let A.Bind(rt, fname) = b in
      let ft = ftype_of_binds b bl in
      let the_function = L.define_function fname ft the_module in
      (* get the ptr to this function *)
      let fp = L.build_alloca (L.pointer_type ft) fname builder in
      ignore(L.build_store the_function fp builder);
      (* ignore(print_endline (L.string_of_lltype (L.type_of the_function))); *)
      (* store the function in global symbol table *)
      (* let _ = Hashtbl.add globalvars fname the_function in *)
      ignore(Hashtbl.add globalvars fname fp);
      (* get the builder *)
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      (* create a new "global" scope with variables outside of current scope *)
      let globals : sbl_tbl = Hashtbl.copy globalvars in
      let _ = Hashtbl.iter (fun k v -> Hashtbl.add globals k v) localvars in
      (* create a "local" scope with all formals *)
      let locals : sbl_tbl = Hashtbl.create 1000 in
      (* build function formals *)
      let create_var (A.Bind(t, n) : A.bind) p = 
        L.set_value_name n p;
        let ty = 
          (match t with
          (* if the formal is a function, return its ptr type *)
          | Ftyp(t, tl) ->
            (* get the function *)
            let ft = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ tl)) in
            (* L.declare_function n ft the_module *)
            L.pointer_type ft
          (* else return the varaible type *)
          | _ -> ltype_of_typ t)
        in
        (* allocate a space for the formal *)
        let local = L.build_alloca ty n fbuilder in
        (* store (initialize) the formal *)
        ignore(L.build_store p local fbuilder);
        Hashtbl.add locals n local
      in
      let _ = List.map2 create_var bl (Array.to_list (L.params the_function)) in
      (* build stmt *)
      let rbuilder = build_stmt globals locals fbuilder the_function loop s in
      (* add return if not: void *)
      let instr = 
        match rt with
        | A.Void -> L.build_ret_void
        | _ -> L.build_ret (L.const_int (ltype_of_typ rt) 0)
      in
      ignore(add_terminal rbuilder instr);
      (* return the original builder *)
      builder

  and build_stmt_list (globalvars : sbl_tbl) (localvars : sbl_tbl) builder the_function loop = function
  | [] -> builder
  | s :: sl -> 
    let b = build_stmt globalvars localvars builder the_function loop s in
    build_stmt_list globalvars localvars b the_function loop sl
  in

  (* symbol tables *)
  let (globals : sbl_tbl) = Hashtbl.create 1000 in
  let (locals : sbl_tbl) = Hashtbl.create 1000 in

  (* main block *)
  let main = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let main_builder = build_stmt_list globals locals builder main None program in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  (* return the module *)
  the_module
