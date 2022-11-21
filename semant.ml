(* Semantic checking for the MicroC compiler *)
open Ast
open Sast

(* module StringMap = Map.Make(String) *)
type tbl_typ = (string, typ) Hashtbl.t

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (program : stmt list) =

  (* let built_in_decls = Hashtbl.create 1000 in *)
  (* let function_decls = Hashtbl.create 1000 in *)
  let (globals : tbl_typ) = Hashtbl.create 1000 in
  let (locals : tbl_typ) = Hashtbl.create 1000 in

  (* Verify a list of bindings has no duplicate names *)
  (* let check_binds (kind : string) (binds : bind list) : ()=
    let rec dups = function
      | [] -> ()
      |	(Bind(_, n1) :: Bind(_, n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in 
    let comp_f b1 b2 = match (b1, b2) with 
      | ((Bind(_, n1)), (Bind(_, n2))) -> compare n1 n2
    in
    dups (List.sort comp_f binds)
  in *)

  (* Collect function declarations for built-in functions: no bodies *)
  Hashtbl.add globals "print" (Ftyp(Int, [Int]));

  (* Add function name to symbol table *)
  (* let add_func fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when Hashtbl.mem built_in_decls n -> make_err built_in_err
    | _ when Hashtbl.mem function_decls n -> make_err dup_err
    | _ ->  Hashtbl.add function_decls n fd; true 
  in *)
  (* let add_func func =
    let Func(Bind(_, name), _, _) = func in
    let built_in_err = "function " ^ name ^ " may not be defined"
    and dup_err = "duplicate function " ^ name
    and make_err er = raise (Failure er)
    and n = name (* Name of the function *) in 
    match func with (* No duplicate functions or redefinitions of built-ins *)
      | _ when Hashtbl.mem built_in_decls n -> make_err built_in_err
      | _ when Hashtbl.mem function_decls n -> make_err dup_err
      | Func(b, bl, s) ->  Hashtbl.add function_decls n (Func(b, bl, s)); true 
  in *)

  (* Collect all function names into one symbol table *)

  (* Return a variable from an input hash table *)
  let type_of_identifier (s : string) (globalvars : tbl_typ) (localvars : tbl_typ) : typ = 
    if Hashtbl.mem localvars s then Hashtbl.find localvars s else
    try Hashtbl.find globalvars s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a list of types from a list of bindings *)
  let rec types_of_binds (binds : bind list) : typ list =
    match binds with
    | [] -> []
    | Bind(x, _) :: t -> x :: types_of_binds t
  in

  (* Return a function from our symbol table *)
  (* let find_func s =
    if Hashtbl.mem function_decls s then Hashtbl.find function_decls s
    else try Hashtbl.find built_in_decls s
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in *)

  (* Raise an exception if the given rvalue type cannot be assigned to
  the given lvalue type *)
  let check_assign (lvaluet : typ) (rvaluet : typ) (err : string) : typ =
    match lvaluet, rvaluet with
    (* regular *)
    | _ when lvaluet = rvaluet -> lvaluet 
    (* lvalue is const while rvalue is non-const *)
    | Const(Int_const), Int -> Const(Int_const)
    | Const(Bool_const), Bool -> Const(Bool_const)
    | Const(Char_const), Char -> Const(Char_const)
    | Const(Float_const), Float -> Const(Float_const)
    | Const(Void_const), Void -> Const(Void_const)
    (* arrays *)
    | _, Arr Void -> lvaluet
    (* error *)
    | _ -> raise (Failure err)
  in

  (* return a semantically checked statement list *)
  let rec check_stmt_list (s : stmt list) (globalvars : tbl_typ) (localvars : tbl_typ) (rettyp : typ) : sstmt list =
    match s with
    | [] -> []
    (* Flatten blocks *)
    (* | Block sl :: sl'  -> check_stmt_list (sl @ sl') globalvars localvars rettyp *)
    | s :: sl -> 
      let a = check_stmt s globalvars localvars rettyp in 
      a :: check_stmt_list sl globalvars localvars rettyp 

  (* Returns (semantically-checked statement, map of t i.e. containing sexprs *)
  and check_stmt (s : stmt) (globalvars : tbl_typ) (localvars : tbl_typ) (rettyp : typ) : sstmt =
    match s with
    (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
    | Block(b) -> SBlock(check_stmt_list b globalvars localvars rettyp)
    | Bstmt(Bind(t, id)) -> 
      if Hashtbl.mem localvars id then raise(Failure ("duplicate variable declaration: " ^ id))
      else let _ = Hashtbl.add localvars id t in
      SBstmt(Bind(t, id))
    | BAstmt(b, e) -> 
      let Bind(t, id) = b in
      let _ = check_stmt (Bstmt(b)) globalvars localvars Void in
      let _, sx = check_expr (Assign(id, e)) globalvars localvars in
      SBAstmt(b, sx)
    | Expr(e) -> SExpr(check_expr e globalvars localvars)
    | If(e, st1, st2) ->
      SIf(check_bool_expr e globalvars localvars, check_stmt st1 globalvars localvars rettyp, check_stmt st2 globalvars localvars rettyp)
    | While(e, st) ->
      SWhile(check_bool_expr e globalvars localvars, check_stmt st globalvars localvars rettyp)
    | For(e1, e2, e3, st) ->
      SFor(check_expr e1 globalvars localvars, check_expr e2 globalvars localvars, check_expr e3 globalvars localvars, check_stmt st globalvars localvars rettyp)
    | Continue -> SContinue
    | Break -> SBreak
    | Func(b, bl, s) as f -> check_func f globalvars localvars
    | Return e -> 
      let (t, e') = check_expr e globalvars localvars in
      if t = rettyp then SReturn (t, e')
      else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                  string_of_typ rettyp ^ " in " ^ string_of_expr e))
      (* TODO: need to check that return val matches declared return val ? maybe done*)

  (* Return a semantically-checked expression, i.e., with a type *)
  and check_expr (ex : expr) (globalvars : tbl_typ) (localvars : tbl_typ) =
    match ex with
    | IntLit l -> (Int, SIntLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | ArrayLit l -> 
      let res = match l with
      | [] -> (Arr Void, SArrayLit [])          (* empty list literal *)
      | hd::tl ->
          let typecheck typ expr = 
            let t, e' = check_expr expr globalvars localvars in
            if t != typ then 
              let err = "inconsistent array " ^ string_of_expr (ArrayLit l) in
              raise(Failure err)
            else (t, e')
          in
          let hd_type, _ = check_expr hd globalvars localvars in
          let listcheck = typecheck hd_type in
          (Arr hd_type, SArrayLit (List.map listcheck l))
        in res
    | Id var -> (type_of_identifier var globalvars localvars, SId var)
    | Assign(var, e) as ex ->
      let ty = type_of_identifier var globalvars localvars
      and (rt, e') = check_expr e globalvars localvars in
      let err = "illegal assignment " ^ string_of_typ ty ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign ty rt err, SAssign(var, (rt, e')))
    | Unop(op, e) -> 
      let t, e' = check_expr e globalvars localvars in 
      let err = "illegal unary operator " ^ 
                string_of_uop op ^ string_of_typ t ^ " in " ^ 
                string_of_expr e
      in
      let ty = match op with
        | Not when t = Bool -> t
        | _ -> raise (Failure err)
      in
      (ty, SUnop(op, (t, e')))
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1 globalvars localvars
      and (t2, e2') = check_expr e2 globalvars localvars in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mul | Div | Mod | BWAnd | BWOr | Exp when t1 = Int -> Int
          | Add | Sub | Mul | Div when t1 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when t1 = Int || t1 = Float || t1 = Bool -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else
        let t = match op with
          Add | Sub | Mul | Div | Exp when t1 = Float && t2 = Int -> Float
          | Add | Sub | Mul | Div when t1 = Int && t2 = Float -> Float
          | Eq | Neq | Less | Greater | Geq | Leq when 
            (t1 = Int || t1 = Float || t1 = Bool) && (t2 = Int || t2 = Float || t2 = Bool) -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
    | Call(fname, args) as call -> check_call fname args call globalvars localvars
    | Noexpr -> (Int, SNoexpr)
    | Subscription(a, e) -> 
      let (t', e') = check_expr e globalvars localvars in
      if t' != Int then 
        let err = "array indicies must be integers, not " ^ string_of_typ t' in
        raise(Failure err)
      else
      (Int, SSubscription(a, (Int, e'))) (* TODO... I think this is done? *)
    | Afunc(rt, bl, s) as f -> check_afunc f globalvars localvars
      (* let f = Func(Bind(rt, "anon"), bl, s) in
      let sf = check_func f globalvars localvars in
      match sf with
      | SFunc(_, _, sbody) -> (Ftyp(rt, types_of_binds bl), SAfunc(rt, bl, sbody))
      | _ -> raise (Failure ("error parsing anonymous function")) *)

  and check_bool_expr e globalvars localvars =
    let (t, e') = check_expr e globalvars localvars in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

  and check_call fname args call globalvars localvars = 
    let ty = 
      if Hashtbl.mem localvars fname then Hashtbl.find localvars fname
      else if Hashtbl.mem globalvars fname then Hashtbl.find globalvars fname
      else raise (Failure ("unrecognized function " ^ fname)) 
    in
      match ty with
      | Ftyp(rt, tl) -> 
        if List.length args != List.length tl then 
          raise (Failure ("expecting " ^ string_of_int (List.length args) ^
          " arguments in " ^ string_of_expr call))
        else let check_c ft e =
          let (et, e') = check_expr e globalvars localvars in
          let err = "illegal argument found " ^ string_of_typ et ^
                    " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
          in (check_assign ft et err, e')
        in
        let args' = List.map2 check_c tl args
        in (rt, SCall(fname, args'))
      | _ -> raise(Failure "invalid call")

  (* return semantically checked function *)
  and check_func func (globalvars : tbl_typ) (localvars : tbl_typ) : sstmt =
    match func with
    | Func(Bind(rt, fname), formals, body) -> 
      let _ = Hashtbl.add globalvars fname (Ftyp(rt, types_of_binds formals)) in
      SFunc(Bind(rt, fname), formals, check_func_decl formals body rt globalvars localvars)
    | _ -> raise (Failure ("error parsing function"))

  and check_afunc func (globalvars : tbl_typ) (localvars : tbl_typ) : sexpr =
    match func with
    | Afunc(rt, formals, body) -> 
      (Ftyp(rt, types_of_binds formals), SAfunc(rt, formals, check_func_decl formals body rt globalvars localvars))
    | _ -> raise (Failure ("error parsing anonymous function"))

  and check_func_decl (formals : bind list) (body : stmt) (rt : typ) (globalvars : tbl_typ) (localvars : tbl_typ) : sstmt = 
    (* Make sure no formals are void or duplicates *)
    (* let _ = check_binds "formals" formals in *)
    (* create a new "global" scope with variables outside of current scope *)
    let globals = Hashtbl.copy globalvars in
    let add_fn k v = Hashtbl.add globals k v in
    let _ = Hashtbl.iter add_fn localvars in
    (* create local scope and fill with formals, locally scoped vars can't be redefined but globals can *)
    let locals = Hashtbl.create 1000 in
    let _ = List.map (fun (Bind(ty, name)) -> Hashtbl.add locals name ty) formals in
    check_stmt body globals locals rt

  in check_stmt_list program globals locals Void 
